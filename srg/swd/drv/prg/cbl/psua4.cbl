       Identification Division.
       Program-Id.                                 psua4              .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    drv                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/01/01    *
      *                       Ultima revisione:    NdK del 26/02/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Driver stampante : PostScript generico modulo verticale oppure *
      *                    orizzontale, a seconda dell'ampiezza riga.  *
      *                                                                *
      *                    Gestisce i formati : A4, A3, A2, A1, A0.    *
      *                                                                *
      * N.B.             : Elementi sperimentali per moduli EPS        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Orientamento     : Stampa con orientamento 'portrait' fino ad  *
      *                    una ampiezza linea di 132 caratteri per poi *
      *                    passare automaticamente a 'landscape'.      *
      *                    In presenza delle opportune opzioni,        *
      *                    l'orientamento puo' essere forzato.         *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Font             : Il font di default e' 'Courier' ma puo'     *
      *                    essere posto in un parametro per il driver  *
      *                    un font differente, purche' supportato da   *
      *                    tutte le stampanti che utilizzano il driver *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Parametri opz.   : "EOT=N" = sequenza End Of Transmission      *
      *                              disattivata                       *
      *                                                                *
      *                    "F=COB" = Font Courier Bold in alternativa  *
      *                              al font standard Courier          *
      *                                                                *
      *                    "F=LGB" = Font Letter Ghotic in alternativa *
      *                              al font standard Courier          *
      *                                                                *
      *                    "L=NNN" = Esclusione cornice della pagina   *
      *                                                                *
      *                    "O=ORI" = Forzatura pagina in orizzonale    *
      *                                                                *
      *                    "O=VER" = Forzatura pagina in vericale      *
      *                                                                *
      *                    "S+R=N" = Sequenza di Save Graphic Context  *
      *                              disattivata                       *
      *                                                                *
      *                    "X=2.1" = Font Scale espresso in decimi di  *
      *                              punto a 2.1 (default 0.9)         *
      *                                                                *
      *                    "E=STP" = Riciclo su scrittura 'stp' in ca- *
      *                              so di errore (server lento)       *
      *                                                                *
      *                                                                *
      *                    N.B.: Max 10 parametri contemporaneamente   *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * EPS              : In presenza di modulo con inclusione EPS    *
      *                    Postscript, il driver provvede all'inclu-   *
      *                    sione leggendo i/il file '.eps' prelevato/i *
      *                    normalmente dalla cartella '/abd/asc/mod' o *
      *                    '/abd/asc/bcd' in caso di barcode. Massimo  *
      *                    36 inclusioni (3 colonne da 12)             *
      *                                                                *
      *                    Da riga 3724 istruzioni per l'inclusione    *
      *                                                                *
      *================================================================*
      *                                                                *
      * Questo e' il modulo base per la stampa PostScript.             *
      *                                                                *
      * Da esso sono ricavati tutti gli altri driver di stampa per le  *
      * stampanti di tipo PostScript, di qualsiasi formato e orienta-  *
      * mento.                                                         *
      *                                                                *
      * I driver di stampa ricavati saranno esattamente identici al    *
      * modulo base, tranne per le forzature della seguente costan-    *
      * te parametrica :                                               *
      *                                                                *
      *  - Limite per la rotazione a 'landscape', ovvero la variabile  *
      *    w-lim-rot.                                                  *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Funziona secondo i seguenti parametri fondamentali, espressi   *
      * in punti, ovvero 72mi di pollice :                             *
      *                                                                *
      *  - Altezza per il tipo stampante                               *
      *     - Se A4 : 0841.8898                                        *
      *     - Se A3 : 1190.5512                                        *
      *     - Se A2 : 1683.7796                                        *
      *     - Se A1 : 2381.1024                                        *
      *     - Se A0 : 3367.5592                                        *
      *                                                                *
      *  - Larghezza per il tipo stampante, forzata a 595.2756, poi-   *
      *     - Se A4 : 0595.2756                                        *
      *     - Se A3 : 0841.8898                                        *
      *     - Se A2 : 1190.5512                                        *
      *     - Se A1 : 1683.7796                                        *
      *     - Se A0 : 2381.1024                                        *
      *                                                                *
      *  - Limite dell'ampiezza stampa in caratteri, come passata dal  *
      *    modulo 'mpslct', al di la' della quale viene effettuata     *
      *    una rotazione di 90 gradi per la stampa in orizzontale,     *
      *    forzata a 132.                                              *
      *                                                                *
      *  - Margine sinistro,  come definito dal tipo stampante.        *
      *                                                                *
      *  - Margine destro,    come definito dal tipo stampante.        *
      *                                                                *
      *  - Margine superiore, come definito dal tipo stampante.        *
      *                                                                *
      *  - Margine inferiore, come definito dal tipo stampante.        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *    Nota : Normalmente i margini per il tipo stampante vengono  *
      *           posti tutti a zero. Cio' equivale a porre dei mar-   *
      *           gini di 50 pixel. In tal modo, visto che la riqua-   *
      *           dratura viene spostata all'esterno di due pixel,     *
      *           avremo  la riquadratura esattamente a 48 pixel dai   *
      *           margini fisici della carta, ovvero a 4/6 di pollice. *
      *                                                                *
      *           Avere sempre la stessa marginatura fisica e' molto   *
      *           importante nel caso di moduli prestampati, poiche'   *
      *           eventuali discordanze sarebbero di immediata evi-    *
      *           denza rispetto alle fincature del tipografo.         *
      *                                                                *
      *           Tutto cio' vale per stampanti PostScript perfette,   *
      *           cioe' che non eseguano spostamenti arbitrari ri-     *
      *           spetto ai margini fisici della carta.                *
      *                                                                *
      *           Nel caso di stampanti non perfette, sara' necessa-   *
      *           rio correggere il tiro, operando le opportune corre- *
      *           zioni sulle definizioni dei margini assegnati al ti- *
      *           po stampante definito, rinunciando al valore di de-  *
      *           fault pari a zero (corrispondente al valore 50), ed  *
      *           inserendo un valore alternativo (probabilmente com-  *
      *           preso tra 40 e 60 a seconda del margine interessato).*
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
      *    * File Control [stp]                                        *
      *    *-----------------------------------------------------------*
           select optional   stp   assign to input-output   f-stp-pat
                             organization is line sequential
                             access  mode is sequential
                             file status  is                f-stp-sts .

      *    *===========================================================*
      *    * File Control [pss]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pss   assign to disk           f-pss-pat
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
      *    * File Description [stp]                                    *
      *    *-----------------------------------------------------------*
       fd  stp       label record omitted                             .
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  stp-rec.
           05  stp-chr occurs 255         pic  x(01)                  .

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
      *    * Work area per trattamento [stp] riguardante il cobol      *
      *    *-----------------------------------------------------------*
       01  f-stp.
      *        *-------------------------------------------------------*
      *        * Cobol file name                                       *
      *        *-------------------------------------------------------*
           05  f-stp-nam                  pic  x(04) value "stp "     .
      *        *-------------------------------------------------------*
      *        * Cobol file pathname                                   *
      *        *-------------------------------------------------------*
           05  f-stp-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Cobol file status                                     *
      *        *-------------------------------------------------------*
           05  f-stp-sts                  pic  x(02)                  .

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
      *    * Work per records di [pss] 'tst'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpsstst0.cpw"                   .

      *    *===========================================================*
      *    * Work per parametri di stampa per driver di stampa         *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wparpds0.cpw"                   .

      *    *===========================================================*
      *    * Work per spaziature orizzontali da driver di stampa       *
      *    *-----------------------------------------------------------*
       01  w-spa-ori.
      *        *-------------------------------------------------------*
      *        * Indice su elementi                                    *
      *        *-------------------------------------------------------*
           05  w-spa-ori-inx              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero elementi presenti                              *
      *        *-------------------------------------------------------*
           05  w-spa-ori-nel              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Elementi 01..20                                       *
      *        *-------------------------------------------------------*
           05  w-spa-ori-ele   occurs 20  pic  9(02)v9(02)            .

      *    *===========================================================*
      *    * Work per spaziature verticali da driver di stampa         *
      *    *-----------------------------------------------------------*
       01  w-spa-ver.
      *        *-------------------------------------------------------*
      *        * Indice su elementi                                    *
      *        *-------------------------------------------------------*
           05  w-spa-ver-inx              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero elementi presenti                              *
      *        *-------------------------------------------------------*
           05  w-spa-ver-nel              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Elementi 01..20                                       *
      *        *-------------------------------------------------------*
           05  w-spa-ver-ele   occurs 20  pic  9(02)v9(02)            .

      *    *===========================================================*
      *    * Work per aree non stampabili                              *
      *    *-----------------------------------------------------------*
       01  w-non-sta.
      *        *-------------------------------------------------------*
      *        * Area non stampabile superiore                         *
      *        *-------------------------------------------------------*
           05  w-non-sta-sup              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Area non stampabile inferiore                         *
      *        *-------------------------------------------------------*
           05  w-non-sta-inf              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Area non stampabile sinistra                          *
      *        *-------------------------------------------------------*
           05  w-non-sta-sin              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Area non stampabile destra                            *
      *        *-------------------------------------------------------*
           05  w-non-sta-des              pic  9(04)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

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
      *    * Work-area per parametri EPS                               *
      *    *-----------------------------------------------------------*
       01  w-par.
      *        *-------------------------------------------------------*
      *        * Comodo per lettura eventuali parametri speciali per   *
      *        * file 'eps'                                            *
      *        *-------------------------------------------------------*
           05  w-par-eps.
      *            *---------------------------------------------------*
      *            * Comodo per trattamento font di stampa             *
      *            *---------------------------------------------------*
               10  w-par-def-fnt          pic  x(30)                  .
      *            *---------------------------------------------------*
      *            * Comodo per trattamento scala font di stampa       *
      *            *---------------------------------------------------*
               10  w-par-def-sfn          pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per trattamento stringa in input           *
      *            *---------------------------------------------------*
               10  w-par-eps-spc          pic  x(99)                  .
      *            *---------------------------------------------------*
      *            * Ridefinizione master                              *
      *            *                                                   *
      *            * Flag di parametri EPS                             *
      *            *                                                   *
      *            *  - '@' : per moduli EPS                           *
      *            *  - '+' : per moduli con piu' EPS                  *
      *            *  - '#' : per moduli EPS in prima pagina, generati *
      *            *          al volo                                  *
      *            *---------------------------------------------------*
               10  w-par-eps-spc-mst  redefines
                   w-par-eps-spc.
                   15  w-par-eps-spc-flg  pic  x(01)                  .
                   15  filler             pic  x(98)                  .
      *            *---------------------------------------------------*
      *            * Ridefinizione '@'                                 *
      *            *                                                   *
      *            * Flag di parametri EPS       - fisso '@'           *
      *            * Interlinea                  - formato 99.99       *
      *            * Scala                       - formato 9.9         *
      *            * Margine superiore           - formato 99          *
      *            * Margine inferiore           - formato 99          *
      *            * Margine sinistro            - formato 99          *
      *            * Margine destro              - formato 99          *
      *            * Posizionamento asse 'x'     - formato 99          *
      *            * Posizionamento asse 'y'     - formato 99          *
      *            * Angolo di rotazione         - formato 999         *
      *            * Linee di stampa per font    - formato 999         *
      *            * Presenza di piu' EPS        - formato X           *
      *            *---------------------------------------------------*
               10  w-par-eps-spc-mdl  redefines
                   w-par-eps-spc.
                   15  w-par-eps-spc-fmd  pic  x(01)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-int  pic  x(05)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-scl  pic  x(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-msu  pic  9(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-min  pic  9(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-msi  pic  9(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-mde  pic  9(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-crx  pic  x(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-cry  pic  x(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-rot  pic  x(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-als  pic  x(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-eps  pic  x(01)                  .
                   15  filler             pic  x(60)                  .
      *            *---------------------------------------------------*
      *            * Ridefinizione '+'                                 *
      *            *                                                   *
      *            * Flag di parametri EPS       - fisso '+'           *
      *            * Numero di linee fisso       - formato 99          *
      *            * Numero di EPS per linea     - formato 9           *
      *            * Correttivo margine sup.     - formato 999         *
      *            * Correttivo linea inf.       - formato 999         *
      *            * Posizione 1. EPS            - formato 999         *
      *            * Scala     1. EPS            - formato 9.9         *
      *            * Posizione 2. EPS            - formato 999         *
      *            * Scala     2. EPS            - formato 9.9         *
      *            * Posizione 3. EPS            - formato 999         *
      *            * Scala     3. EPS            - formato 9.9         *
      *            * Scala     per il font       - formato 9.9         *
      *            *---------------------------------------------------*
               10  w-par-eps-spc-img  redefines
                   w-par-eps-spc.
                   15  w-par-eps-spc-fim  pic  x(01)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-nlf  pic  9(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-nel  pic  9(01)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-cma  pic  9(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-cli  pic  9(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-pe1  pic  x(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-se1  pic  x(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-pe2  pic  x(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-se2  pic  x(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-pe3  pic  x(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-se3  pic  x(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-sfn  pic  x(03)                  .
                   15  filler             pic  x(57)                  .
      *            *---------------------------------------------------*
      *            * Ridefinizione '#'                                 *
      *            *                                                   *
      *            * Flag di parametri EPS         fisso '#'           *
      *            * Pagina per lettura campo      formato 999         *
      *            * Linea  per lettura campo      formato 999         *
      *            * Posizione per lettura campo   formato 999         *
      *            * Lunghezza valore da estrarre  formato 999         *
      *            * Posizione EPS                 formato 999         *
      *            * Scala     EPS                 formato 9.9         *
      *            * Scala     per il font         formato 9.9         *
      *            *---------------------------------------------------*
               10  w-par-eps-spc-bcd  redefines
                   w-par-eps-spc.
                   15  w-par-eps-spc-fbc  pic  x(01)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-pag  pic  9(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-lin  pic  9(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-pos  pic  9(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-lun  pic  9(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-xxx  pic  9(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-yyy  pic  9(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-sep  pic  x(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-par-eps-spc-sef  pic  x(03)                  .
                   15  filler             pic  x(66)                  .
      *            *---------------------------------------------------*
      *            * Comodi di lavoro                                  *
      *            *---------------------------------------------------*
               10  w-par-eps-wrk.
                   15  w-par-eps-wrk-int  pic  x(05)                  .
                   15  w-par-eps-wrk-scl  pic  x(03)                  .
                   15  w-par-eps-wrk-msu  pic  9(02)                  .
                   15  w-par-eps-wrk-min  pic  9(02)                  .
                   15  w-par-eps-wrk-msi  pic  9(02)                  .
                   15  w-par-eps-wrk-mde  pic  9(02)                  .
                   15  w-par-eps-wrk-crx  pic  x(02)                  .
                   15  w-par-eps-wrk-cry  pic  x(02)                  .
                   15  w-par-eps-wrk-rot  pic  x(03)                  .
                   15  w-par-eps-wrk-als  pic  x(03)                  .
                   15  w-par-eps-wrk-nlf  pic  9(02)                  .
                   15  w-par-eps-wrk-nel  pic  9(01)                  .
                   15  w-par-eps-wrk-cma  pic  9(03)                  .
                   15  w-par-eps-wrk-cli  pic  9(03)                  .
                   15  w-par-eps-wrk-pe1  pic  x(03)                  .
                   15  w-par-eps-wrk-se1  pic  x(03)                  .
                   15  w-par-eps-wrk-pe2  pic  x(03)                  .
                   15  w-par-eps-wrk-se2  pic  x(03)                  .
                   15  w-par-eps-wrk-pe3  pic  x(03)                  .
                   15  w-par-eps-wrk-se3  pic  x(03)                  .
                   15  w-par-eps-wrk-sfn  pic  x(03)                  .
                   15  w-par-eps-wrk-lfl  pic  9(03)                  .
                   15  w-par-eps-wrk-lin  pic  9(03)                  .
                   15  w-par-eps-wrk-pos  pic  9(03)                  .
                   15  w-par-eps-wrk-lun  pic  9(03)                  .
                   15  w-par-eps-wrk-xxx  pic  9(03)                  .
                   15  w-par-eps-wrk-yyy  pic  9(03)                  .
                   15  w-par-eps-wrk-sep  pic  x(04)                  .
                   15  w-par-eps-wrk-sef  pic  x(04)                  .
                   15  w-par-eps-wrk-num  pic  x(06)                  .
                   15  w-par-eps-wrk-npg  pic  x(06)                  .
                   15  w-par-eps-wrk-ctb  pic  9(05)                  .
                   15  w-par-eps-wrk-flg  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione EPS per pagina              *
      *    *-----------------------------------------------------------*
       01  w-buf.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-buf-num-ele              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-buf-max-ele              pic  9(03) value 12         .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-buf-tbl.
               10  w-buf-sng-ele occurs 12.
                   15  w-buf-eps-pth  occurs 03
                                          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per posizionamento e scala EPS                 *
      *        *-------------------------------------------------------*
           05  w-buf-xxx-eps              pic  9(03)                  .
           05  w-buf-yyy-eps              pic  9(03)                  .
           05  w-buf-scl-eps              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per estrazione                                 *
      *        *-------------------------------------------------------*
           05  w-buf-ctr-001              pic  9(03)                  .
           05  w-buf-ctr-002              pic  9(03)                  .
           05  w-buf-wrk-alf              pic  x(240)                 .
           05  w-buf-wrk-al1              pic  x(240)                 .
           05  w-buf-wrk-al2              pic  x(240)                 .
           05  w-buf-wrk-ext              pic  x(80)                  .
           05  w-buf-wrk-pt1              pic  x(40)                  .
           05  w-buf-wrk-pt2              pic  x(40)                  .
           05  w-buf-wrk-pt3              pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per il driver                                   *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Work per flags passati dal modulo stampa al momento   *
      *        * del richiamo della Open del driver                    *
      *        *-------------------------------------------------------*
           05  w-fpm.
      *            *---------------------------------------------------*
      *            * Tipo stampa                                       *
      *            * - Spaces : diretta                                *
      *            * - S      : in spool                               *
      *            *---------------------------------------------------*
               10  w-dos                  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Inibizione del salto pagina finale in caso  di    *
      *            * stampa in spool                                   *
      *            * - N : Non inibito                                 *
      *            * - S : Inibito                                     *
      *            *---------------------------------------------------*
               10  w-fff                  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore per numero linea della pagina               *
      *        *-------------------------------------------------------*
           05  w-lin                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio dello i-o status               *
      *        *-------------------------------------------------------*
           05  w-sav                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagine nel documento PostScript                *
      *        *-------------------------------------------------------*
           05  w-nps                      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per determinazione pathname file 'eps'         *
      *        *-------------------------------------------------------*
           05  w-pat-eps.
               10  w-pat-eps-pbs          pic  x(40)                  .
               10  w-pat-eps-pat          pic  x(40)                  .
               10  w-pat-eps-nam          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per ridefinizione system date and time           *
      *        *-------------------------------------------------------*
           05  w-sdt                      pic  9(15)                  .
           05  w-sdt-r01 redefines w-sdt.
               10  w-sdt-ann              pic  9(03)                  .
               10  w-sdt-mes              pic  9(02)                  .
               10  w-sdt-gio              pic  9(02)                  .
               10  w-sdt-ora              pic  9(02)                  .
               10  w-sdt-min              pic  9(02)                  .
               10  w-sdt-sec              pic  9(02)                  .
               10  w-sdt-cnt              pic  9(02)                  .
           05  w-sdt-r02 redefines w-sdt.
               10  w-sdt-xxx occurs 15    pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per commento 'CreationDate'                      *
      *        *-------------------------------------------------------*
           05  w-crd.
               10  w-crd-mmm              pic  9(02)                  .
               10  w-crd-t01              pic  x(01) value "-"        .
               10  w-crd-ddd              pic  9(02)                  .
               10  w-crd-t02              pic  x(01) value "-"        .
               10  w-crd-yyy              pic  9(04)                  .
               10  w-crd-t03              pic  x(01) value spaces     .
               10  w-crd-hhh              pic  9(02)                  .
               10  w-crd-t04              pic  x(01) value ":"        .
               10  w-crd-min              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per il valore del limite di rotazione            *
      *        *-------------------------------------------------------*
           05  w-lim-rot                  pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per il valore dell'altezza stampante             *
      *        *-------------------------------------------------------*
           05  w-alt-stp.
               10  w-alt-stp-int          pic  9(04)                  .
               10  filler                 pic  x(01)       value "."  .
               10  w-alt-stp-dec          pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Work per il valore della larghezza stampante          *
      *        *-------------------------------------------------------*
           05  w-lar-stp.
               10  w-lar-stp-int          pic  9(04)                  .
               10  filler                 pic  x(01)       value "."  .
               10  w-lar-stp-dec          pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per il settaggio del numero linee              *
      *        *-------------------------------------------------------*
           05  w-set-nrl                  pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per il settaggio dei margini                   *
      *        *-------------------------------------------------------*
           05  w-set-msu                  pic  9(04)                  .
           05  w-set-min                  pic  9(04)                  .
           05  w-set-mde                  pic  9(04)                  .
           05  w-set-msi                  pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per il settaggio di altezza e larghezza        *
      *        *-------------------------------------------------------*
           05  w-set-4v4                  pic  x(09)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per inclusione EPS                             *
      *        *-------------------------------------------------------*
           05  w-set-fin                  pic  9(04)                  .
           05  w-set-top                  pic  9(04)                  .
           05  w-set-wtp                  pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per il riciclo in scrittura file 'stp'         *
      *        *-------------------------------------------------------*
           05  w-set-ctr                  pic  9(01)                  .

      *    *===========================================================*
      *    * Work per manipolazione di valori binari                   *
      *    *-----------------------------------------------------------*
       01  w-bin.
      *        *-------------------------------------------------------*
      *        * Indice per selezione di w-bin-byt                     *
      *        *-------------------------------------------------------*
           05  w-bin-inx                  pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore numerico da 000 a 255                          *
      *        *-------------------------------------------------------*
           05  w-bin-num                  pic s9(04)       comp-1     .
           05  w-bin-nur redefines
               w-bin-num.
      *            *---------------------------------------------------*
      *            * Byte corrispondente al valore numerico            *
      *            *---------------------------------------------------*
               10  w-bin-byt occurs 2     pic  x(01)                  .

      *    *===========================================================*
      *    * Work per caratteri ASCII di controllo                     *
      *    *-----------------------------------------------------------*
       01  w-asc.
      *        *-------------------------------------------------------*
      *        * C0 Controls Characters                                *
      *        *-------------------------------------------------------*
           05  w-asc-c0c.
      *            *---------------------------------------------------*
      *            * End of transmission                               *
      *            *---------------------------------------------------*
               10  w-c0c-EOT              pic  x(01)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione barcode          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/dbarcod0.dtl"                   .

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
       Decl Section.
           Use after standard error procedure on stp.
       decl-000.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status contenuto  *
      *              * in f-stp-sts nel codice di i-o status conven-   *
      *              * zionale                                         *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-stp                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-stp-sts            to   e-sts                  .
       End Declaratives.

      *================================================================*
      *    Main                                                        *
      *================================================================*
       Main Section.
       main-000.
      *              *-------------------------------------------------*
      *              * Open                                            *
      *              *-------------------------------------------------*
           if        p-sub                =    "OO"
                     perform opn-out-000  thru opn-out-999
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           else if   p-sub                =    "CO"
                     perform cls-out-000  thru cls-out-999
      *              *-------------------------------------------------*
      *              * Write Page                                      *
      *              *-------------------------------------------------*
           else if   p-sub                =    "WP"
                     perform wrt-pag-000  thru wrt-pag-999
      *              *-------------------------------------------------*
      *              * Eject                                           *
      *              *-------------------------------------------------*
           else if   p-sub                =    "EJ"
                     perform ejc-pag-000  thru ejc-pag-999
      *              *-------------------------------------------------*
      *              * Spool-Open                                      *
      *              *-------------------------------------------------*
           else if   p-sub                =    "SO"
                     perform spl-opn-000  thru spl-opn-999
      *              *-------------------------------------------------*
      *              * Spool-Close                                     *
      *              *-------------------------------------------------*
           else if   p-sub                =    "SC"
                     perform spl-cls-000  thru spl-cls-999
      *              *-------------------------------------------------*
      *              * Spool-Delete                                    *
      *              *-------------------------------------------------*
           else if   p-sub                =    "SD"
                     perform spl-dlt-000  thru spl-dlt-999            .
       main-999.
           exit      program                                          .

      *    *===========================================================*
      *    * Open Output                                               *
      *    *-----------------------------------------------------------*
       opn-out-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione flags passati dal modulo stampa  *
      *              *-------------------------------------------------*
           move      p-alf                to   w-fpm                  .
      *              *-------------------------------------------------*
      *              * Determinazione indice per selezione w-bin-byt   *
      *              *-------------------------------------------------*
           perform   det-byt-inx-000      thru det-byt-inx-999        .
      *              *-------------------------------------------------*
      *              * Determinazione parametri per il driver stampa   *
      *              *-------------------------------------------------*
           perform   let-tst-pss-000      thru let-tst-pss-999        .
      *              *-------------------------------------------------*
      *              * Determinazione C0 Control Characters            *
      *              *-------------------------------------------------*
           perform   det-asc-c0c-000      thru det-asc-c0c-999        .
      *              *-------------------------------------------------*
      *              * Determinazione pathname file [stp]              *
      *              *-------------------------------------------------*
           perform   det-pat-stp-000      thru det-pat-stp-999        .
      *              *-------------------------------------------------*
      *              * Open output file [stp]                          *
      *              *-------------------------------------------------*
           perform   opo-stp-000          thru opo-stp-999            .
      *              *-------------------------------------------------*
      *              * Se errori in open output [stp] : uscita         *
      *              *-------------------------------------------------*
           if        p-rsc                not  =  spaces
                     go to opn-out-999.
      *              *-------------------------------------------------*
      *              * Emissione sequenze di controllo iniziali        *
      *              *-------------------------------------------------*
           perform   emi-seq-ini-000      thru emi-seq-ini-999        .
           if        p-rsc                not  = spaces
                     go to opn-out-999.
       opn-out-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-out-000.
      *              *-------------------------------------------------*
      *              * Se errori : uscita                              *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to cls-out-999.
      *              *-------------------------------------------------*
      *              * Scaricamento pagina in sospeso                  *
      *              *-------------------------------------------------*
      *    perform   wrt-pag-000          thru wrt-pag-999            .
      *              *-------------------------------------------------*
      *              * Se errori : uscita                              *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to cls-out-999.
      *              *-------------------------------------------------*
      *              * Emissione sequenze di controllo finali          *
      *              *-------------------------------------------------*
           perform   emi-seq-fin-000      thru emi-seq-fin-999        .
      *              *-------------------------------------------------*
      *              * Se errori : uscita                              *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to cls-out-999.
      *              *-------------------------------------------------*
      *              * Close file [stp]                                *
      *              *-------------------------------------------------*
           perform   cls-stp-000          thru cls-stp-999            .
      *              *-------------------------------------------------*
      *              * Se errori in close [stp] : uscita               *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to cls-out-999.
       cls-out-999.
           exit.

      *    *===========================================================*
      *    * Write Page                                                *
      *    *-----------------------------------------------------------*
       wrt-pag-000.
      *              *-------------------------------------------------*
      *              * Se la pagina e' completamente a spaces la si i- *
      *              * gnora                                           *
      *              *-------------------------------------------------*
           if        p-buf                =    spaces
                     go to wrt-pag-999.
      *              *-------------------------------------------------*
      *              * Emissione sequenze di inizio pagina             *
      *              *-------------------------------------------------*
           perform   emi-seq-inp-000      thru emi-seq-inp-999        .
      *              *-------------------------------------------------*
      *              * Se errori : uscita                              *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to wrt-pag-999.
       wrt-pag-200.
      *              *-------------------------------------------------*
      *              * Top pagina                                      *
      *              *-------------------------------------------------*
       wrt-pag-210.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-lin                  .
       wrt-pag-220.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-lin                  .
      *                  *---------------------------------------------*
      *                  * Test se fine Top                            *
      *                  *---------------------------------------------*
           if        w-lin                >    p-sel-eft-sel
                     go to wrt-pag-400.
      *                  *---------------------------------------------*
      *                  * Scrittura linea a spaces                    *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                  *---------------------------------------------*
      *                  * Se avvenuti errori in scrittura : uscita    *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     go to wrt-pag-999.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     wrt-pag-220.
       wrt-pag-400.
      *              *-------------------------------------------------*
      *              * Corpo pagina                                    *
      *              *-------------------------------------------------*
       wrt-pag-410.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-lin                  .
       wrt-pag-420.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-lin                  .
      *                  *---------------------------------------------*
      *                  * Test se fine Corpo                          *
      *                  *---------------------------------------------*
           if        w-lin                >    p-sel-efc-sel
                     go to wrt-pag-600.
      *                  *---------------------------------------------*
      *                  * Scrittura linea                             *
      *                  *---------------------------------------------*
           perform   wrt-lin-000          thru wrt-lin-999            .
      *                  *---------------------------------------------*
      *                  * Se avvenuti errori in scrittura : uscita    *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     go to wrt-pag-999.
      *                  *---------------------------------------------*
      *                  * Riciclo a linea successiva                  *
      *                  *---------------------------------------------*
           go to     wrt-pag-420.
       wrt-pag-600.
      *              *-------------------------------------------------*
      *              * Bottom pagina                                   *
      *              *-------------------------------------------------*
       wrt-pag-610.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-lin                  .
       wrt-pag-620.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-lin                  .
      *                  *---------------------------------------------*
      *                  * Test se fine Bottom                         *
      *                  *---------------------------------------------*
           if        w-lin                >    p-sel-efb-sel
                     go to wrt-pag-800.
      *                  *---------------------------------------------*
      *                  * Scrittura linea a spaces                    *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                  *---------------------------------------------*
      *                  * Se avvenuti errori in scrittura : uscita    *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     go to wrt-pag-999.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     wrt-pag-620.
       wrt-pag-800.
      *              *-------------------------------------------------*
      *              * Fine pagina                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione sequenze di fine pagina           *
      *                  *---------------------------------------------*
           perform   emi-seq-fip-000      thru emi-seq-fip-999        .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita                          *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     go to wrt-pag-999.
       wrt-pag-999.
           exit.

      *    *===========================================================*
      *    * Eject Page                                                *
      *    *-----------------------------------------------------------*
       ejc-pag-000.
      *              *-------------------------------------------------*
      *              * Write page                                      *
      *              *-------------------------------------------------*
           perform   wrt-pag-000          thru wrt-pag-999            .
       ejc-pag-999.
           exit.

      *    *===========================================================*
      *    * Spool-Open                                                *
      *    *-----------------------------------------------------------*
       spl-opn-000.
      *              *-------------------------------------------------*
      *              * Se errori : uscita                              *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to spl-opn-999.
      *              *-------------------------------------------------*
      *              * Open output file [stp]                          *
      *              *-------------------------------------------------*
           perform   opo-stp-000          thru opo-stp-999            .
      *              *-------------------------------------------------*
      *              * Se errori in open output [stp] : uscita         *
      *              *-------------------------------------------------*
           if        p-rsc                not  =  spaces
                     go to spl-opn-999.
      *              *-------------------------------------------------*
      *              * Emissione sequenze di controllo iniziali        *
      *              *-------------------------------------------------*
           perform   emi-seq-ini-000      thru emi-seq-ini-999        .
      *              *-------------------------------------------------*
      *              * Se errori : uscita                              *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to spl-opn-999.
       spl-opn-999.
           exit.

      *    *===========================================================*
      *    * Spool-Close                                               *
      *    *-----------------------------------------------------------*
       spl-cls-000.
      *              *-------------------------------------------------*
      *              * Se errori : uscita                              *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to spl-cls-999.
      *              *-------------------------------------------------*
      *              * Emissione sequenze di controllo finali          *
      *              *-------------------------------------------------*
           perform   emi-seq-fin-000      thru emi-seq-fin-999        .
      *              *-------------------------------------------------*
      *              * Se errori : uscita                              *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to spl-cls-999.
      *              *-------------------------------------------------*
      *              * Close file [stp]                                *
      *              *-------------------------------------------------*
           perform   cls-stp-000          thru cls-stp-999            .
      *              *-------------------------------------------------*
      *              * Se errori in close [stp] : uscita               *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to spl-cls-999.
       spl-cls-999.
           exit.

      *    *===========================================================*
      *    * Spool-Delete                                              *
      *    *-----------------------------------------------------------*
       spl-dlt-000.
      *              *-------------------------------------------------*
      *              * Se errori : uscita                              *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to spl-dlt-999.
      *              *-------------------------------------------------*
      *              * Delete file [stp]                               *
      *              *-------------------------------------------------*
           perform   dlt-stp-000          thru dlt-stp-999            .
      *              *-------------------------------------------------*
      *              * Se errori in close [stp] : uscita               *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to spl-dlt-999.
       spl-dlt-999.
           exit.

      *    *===========================================================*
      *    * Write Line                                                *
      *    *-----------------------------------------------------------*
       wrt-lin-000.
      *              *-------------------------------------------------*
      *              * Composizione record di [stp] da scrivere        *
      *              *-------------------------------------------------*
           move      p-buf-lin (w-lin)    to   stp-rec                .
      *              *-------------------------------------------------*
      *              * Scrittura record di [stp]                       *
      *              *-------------------------------------------------*
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *              *-------------------------------------------------*
      *              * Se errori : uscita                              *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     go to wrt-lin-999.
       wrt-lin-999.
           exit.

      *    *===========================================================*
      *    * Apertura file [stp] in output                             *
      *    *-----------------------------------------------------------*
       opo-stp-000.
      *              *-------------------------------------------------*
      *              * Open output                                     *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           open      output stp                                       .
      *              *-------------------------------------------------*
      *              * Set dello status di errore se necessario        *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     move  e-sts          to   p-rsc                  .
       opo-stp-999.
           exit.

      *    *===========================================================*
      *    * Chiusura file [stp]                                       *
      *    *-----------------------------------------------------------*
       cls-stp-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           close     stp                                              .
      *              *-------------------------------------------------*
      *              * Set dello status di errore se necessario        *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     move  e-sts          to   p-rsc                  .
       cls-stp-999.
           exit.

      *    *===========================================================*
      *    * Delete file [stp]                                         *
      *    *-----------------------------------------------------------*
       dlt-stp-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           delete    file   stp                                       .
      *              *-------------------------------------------------*
      *              * Set dello status di errore se necessario        *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     move  e-sts          to   p-rsc                  .
       dlt-stp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione pathname file [stp]                        *
      *    *-----------------------------------------------------------*
       det-pat-stp-000.
      *              *-------------------------------------------------*
      *              * Canale di uscita della stampante                *
      *              *-------------------------------------------------*
           move      p-sel-can-stp        to   f-stp-pat              .
       det-pat-stp-999.
           exit.

      *    *===========================================================*
      *    * Scrittura effettiva record [stp]                          *
      *    *-----------------------------------------------------------*
       wrt-rec-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore di comodo             *
      *              *-------------------------------------------------*
           move      zero                 to   w-set-ctr              .
      *              *-------------------------------------------------*
      *              * Normalizzazione i-o status                      *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
      *              *-------------------------------------------------*
      *              * Write                                           *
      *              *-------------------------------------------------*
           write     stp-rec                                          .
      *              *-------------------------------------------------*
      *              * Se errori in write                              *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to wrt-rec-stp-999.
       wrt-rec-stp-100.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ricerca tra i parametri stampa associati al *
      *                  * tipo stampante, per vedere se esiste il pa- *
      *                  * rametro E=STP. Se si : si ricicla in scrit- *
      *                  * tura file 'stp' per un massimo di 3 volte   *
      *                  *---------------------------------------------*
           move      "E=STP"              to   w-par-pds-par          .
           perform   ric-par-pds-000      thru ric-par-pds-999        .
           if        w-par-pds-fdr        not  = "S"
                     go to wrt-rec-stp-200.
      *                  *---------------------------------------------*
      *                  * Riciclo per lettura file 'stp'              *
      *                  *---------------------------------------------*
           add       1                    to   w-set-ctr              .
           if        w-set-ctr            >    3
                     go to wrt-rec-stp-200.
      *                  *---------------------------------------------*
      *                  * Attesa di un secondo                        *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mwait0"                         .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     wrt-rec-stp-100.
       wrt-rec-stp-200.
      *                  *---------------------------------------------*
      *                  * Salvataggio i-o status                      *
      *                  *---------------------------------------------*
           move      f-stp-sts            to   w-sav                  .
      *                  *---------------------------------------------*
      *                  * Chiusura file [stp]                         *
      *                  *---------------------------------------------*
           perform   cls-stp-000          thru cls-stp-999            .
      *                  *---------------------------------------------*
      *                  * Ripristino i-o status                       *
      *                  *---------------------------------------------*
           move      w-sav                to   f-stp-sts
                                               e-sts                  .
      *                  *---------------------------------------------*
      *                  * Set dello status di errore                  *
      *                  *---------------------------------------------*
           move      e-sts                to   p-rsc                  .
       wrt-rec-stp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione indice per selezione di w-bin-byt          *
      *    *-----------------------------------------------------------*
       det-byt-inx-000.
      *              *-------------------------------------------------*
      *              * Il campo numerico w-bin-num, essendo stato de-  *
      *              * finito di USAGE COMP-1, impegna due bytes  di   *
      *              * memoria per rappresentare un valore numerico    *
      *              * che puo' essere compreso tra -32768 e +32767 .  *
      *              *                                                 *
      *              * Esistono due tipi di macchine :                 *
      *              * - Normal   byte machine                         *
      *              * - Reversed byte machines                        *
      *              *                                                 *
      *              * L'architettura hardware delle 'normal byte ma-  *
      *              * chines' fa' si' che il byte di sinistra conten- *
      *              * ga gli otto bits piu' significativi e che il    *
      *              * byte di sinistra contenga gli otto bits meno    *
      *              * significativi.                                  *
      *              *                                                 *
      *              * L'architettura hardware delle 'reversed byte    *
      *              * machines' fa' si' che il byte di sinistra con-  *
      *              * tenga gli otto bits meno significativi e che il *
      *              * byte di sinistra contenga gli otto bits piu'    *
      *              * significativi.                                  *
      *              *                                                 *
      *              * Per determinare il tipo di architettura hardwa- *
      *              * re che si sta' usando e' pertanto necessario    *
      *              * provare a memorizzare un numero di formato noto *
      *              * per poi controllare su quale dei due bytes esso *
      *              * sia stato effettivamente memorizzato.           *
      *              *                                                 *
      *              * Il numero che si utilizza e' il numero '1', il  *
      *              * cui formato binario e' sicuramente :            *
      *              *                                                 *
      *              *                             byte    byte        *
      *              *                           <--sx--><--dx-->      *
      *              * - normal   byte machine : 0000000000000001      *
      *              * - reversed byte machine : 0000000100000000      *
      *              *                                                 *
      *              * Quindi si controlla quale byte contiene il va-  *
      *              * lore low-value (00000000) per determinare l'in- *
      *              * dice dell'altro byte, che e' quello interessato *
      *              *                                                 *
      *              *-------------------------------------------------*
           move      1                    to   w-bin-num              .
           if        w-bin-byt (1)        =    low-value
                     move  2              to   w-bin-inx
           else      move  1              to   w-bin-inx              .
       det-byt-inx-999.
           exit.

      *    *===========================================================*
      *    * Determinazione C0 Control Characters                      *
      *    *-----------------------------------------------------------*
       det-asc-c0c-000.
      *              *-------------------------------------------------*
      *              * Valore binario 004                              *
      *              *-------------------------------------------------*
           move      004                  to   w-bin-num              .
           move      w-bin-byt(w-bin-inx) to   w-c0c-EOT              .
       det-asc-c0c-999.
           exit.

      *    *===========================================================*
      *    * Lettura record di [pss] per :                             *
      *    *  - Spaziature orizzontali                                 *
      *    *  - Parametri di stampa associati alla fase gestionale     *
      *    *-----------------------------------------------------------*
       let-tst-pss-000.
      *              *-------------------------------------------------*
      *              * Open file [pss]                                 *
      *              *-------------------------------------------------*
           perform   opn-fil-pss-000      thru opn-fil-pss-999        .
      *              *-------------------------------------------------*
      *              * Se errori : a normalizzazione record [tst]      *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to let-tst-pss-400.
       let-tst-pss-100.
      *              *-------------------------------------------------*
      *              * Preparazione chiave                             *
      *              *-------------------------------------------------*
           move      "tst "               to   pss-tre                .
           move      p-sel-tip-stp        to   pss-kre                .
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      pss    with no lock
                            invalid key
                            go to   let-tst-pss-300.
      *              *-------------------------------------------------*
      *              * Se record bloccato : riciclo alla lettura fino  *
      *              * allo sblocco                                    *
      *              *-------------------------------------------------*
           if        e-sts                =    e-use-err
                     go to let-tst-pss-100.
      *              *-------------------------------------------------*
      *              * Se errori : a chiusura e normalizzazione record *
      *              * [tst]                                           *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to let-tst-pss-300.
       let-tst-pss-200.
      *              *-------------------------------------------------*
      *              * Close file [pss]                                *
      *              *-------------------------------------------------*
           perform   cls-fil-pss-000      thru cls-fil-pss-999        .
      *              *-------------------------------------------------*
      *              * Record letto in area di ridefinizione           *
      *              *-------------------------------------------------*
           move      pss-dat              to   w-tst                  .
      *              *-------------------------------------------------*
      *              * A memorizzazione parametri letti                *
      *              *-------------------------------------------------*
           go to     let-tst-pss-500.
       let-tst-pss-300.
      *              *-------------------------------------------------*
      *              * Close file [pss]                                *
      *              *-------------------------------------------------*
           perform   cls-fil-pss-000      thru cls-fil-pss-999        .
       let-tst-pss-400.
      *              *-------------------------------------------------*
      *              * Normalizzazioni se record non esistente         *
      *              *-------------------------------------------------*
       let-tst-pss-410.
      *                  *---------------------------------------------*
      *                  * Sottoclasse tipo stampante                  *
      *                  *---------------------------------------------*
           move      "A4  "               to   w-tst-scl-tst          .
       let-tst-pss-420.
      *                  *---------------------------------------------*
      *                  * Spaziature orizzontali                      *
      *                  *---------------------------------------------*
       let-tst-pss-421.
           move      zero                 to   w-spa-ori-inx          .
       let-tst-pss-422.
           add       1                    to   w-spa-ori-inx          .
           if        w-spa-ori-inx        >    20
                     go to let-tst-pss-430.
           move      zero                 to   w-tst-spa-ori
                                              (w-spa-ori-inx)         .
           go to     let-tst-pss-422.
       let-tst-pss-430.
      *                  *---------------------------------------------*
      *                  * Spaziature verticali                        *
      *                  *---------------------------------------------*
       let-tst-pss-431.
           move      zero                 to   w-spa-ver-inx          .
       let-tst-pss-432.
           add       1                    to   w-spa-ver-inx          .
           if        w-spa-ver-inx        >    20
                     go to let-tst-pss-440.
           move      zero                 to   w-tst-spa-ver
                                              (w-spa-ver-inx)         .
           go to     let-tst-pss-432.
       let-tst-pss-440.
      *                  *---------------------------------------------*
      *                  * Parametri di stampa                         *
      *                  *---------------------------------------------*
       let-tst-pss-441.
           move      zero                 to   w-par-pds-inx          .
       let-tst-pss-442.
           add       1                    to   w-par-pds-inx          .
           if        w-par-pds-inx        >    10
                     go to let-tst-pss-450.
           move      spaces               to   w-tst-par-pds
                                              (w-par-pds-inx)         .
           go to     let-tst-pss-442.
       let-tst-pss-450.
      *                  *---------------------------------------------*
      *                  * Aree non stampabili                         *
      *                  *  - Superiore                                *
      *                  *  - Inferiore                                *
      *                  *  - Sinistra                                 *
      *                  *  - Destra                                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-tst-nsu-tst          .
           move      zero                 to   w-tst-nin-tst          .
           move      zero                 to   w-tst-nsi-tst          .
           move      zero                 to   w-tst-nde-tst          .
       let-tst-pss-500.
      *              *-------------------------------------------------*
      *              * Memorizzazione sottoclasse tipo stampante       *
      *              *-------------------------------------------------*
           move      w-tst-scl-tst        to   w-par-scl-psc          .
           if        w-par-scl-psc        not  = "A4  " and
                     w-par-scl-psc        not  = "A3  " and
                     w-par-scl-psc        not  = "A2  " and
                     w-par-scl-psc        not  = "A1  " and
                     w-par-scl-psc        not  = "A0  "
                     move  "A4  "         to   w-par-scl-psc          .
       let-tst-pss-550.
      *              *-------------------------------------------------*
      *              * Memorizzazione spaziature orizzontali           *
      *              *-------------------------------------------------*
       let-tst-pss-552.
           move      zero                 to   w-spa-ori-inx          .
           move      zero                 to   w-spa-ori-nel          .
       let-tst-pss-554.
           add       1                    to   w-spa-ori-inx          .
           if        w-spa-ori-inx        >    20
                     go to let-tst-pss-600.
           if        w-tst-spa-ori
                    (w-spa-ori-inx)       =    zero
                     go to let-tst-pss-554.
           add       1                    to   w-spa-ori-nel          .
           move      w-tst-spa-ori
                    (w-spa-ori-inx)       to   w-spa-ori-ele
                                              (w-spa-ori-nel)         .
           go to     let-tst-pss-554.
       let-tst-pss-600.
      *              *-------------------------------------------------*
      *              * Memorizzazione spaziature verticali             *
      *              *-------------------------------------------------*
       let-tst-pss-602.
           move      zero                 to   w-spa-ver-inx          .
           move      zero                 to   w-spa-ver-nel          .
       let-tst-pss-604.
           add       1                    to   w-spa-ver-inx          .
           if        w-spa-ver-inx        >    20
                     go to let-tst-pss-650.
           if        w-tst-spa-ver
                    (w-spa-ver-inx)       =    zero
                     go to let-tst-pss-604.
           add       1                    to   w-spa-ver-nel          .
           move      w-tst-spa-ver
                    (w-spa-ver-inx)       to   w-spa-ver-ele
                                              (w-spa-ver-nel)         .
           go to     let-tst-pss-604.
       let-tst-pss-650.
      *              *-------------------------------------------------*
      *              * Memorizzazione parametri di stampa              *
      *              *-------------------------------------------------*
       let-tst-pss-652.
           move      zero                 to   w-par-pds-inx          .
           move      zero                 to   w-par-pds-nel          .
       let-tst-pss-654.
           add       1                    to   w-par-pds-inx          .
           if        w-par-pds-inx        >    10
                     go to let-tst-pss-700.
           if        w-tst-par-pds
                    (w-par-pds-inx)       =    spaces
                     go to let-tst-pss-654.
           add       1                    to   w-par-pds-nel          .
           move      w-tst-par-pds
                    (w-par-pds-inx)       to   w-par-pds-ele
                                              (w-par-pds-nel)         .
           go to     let-tst-pss-654.
       let-tst-pss-700.
      *              *-------------------------------------------------*
      *              * Aree non stampabili                             *
      *              *  - Superiore                                    *
      *              *  - Inferiore                                    *
      *              *  - Sinistra                                     *
      *              *  - Destra                                       *
      *              *-------------------------------------------------*
           if        w-tst-nsu-tst        not  numeric
                     move  zero           to   w-non-sta-sup
           else      move  w-tst-nsu-tst  to   w-non-sta-sup          .
           if        w-tst-nin-tst        not  numeric
                     move  zero           to   w-non-sta-inf
           else      move  w-tst-nin-tst  to   w-non-sta-inf          .
           if        w-tst-nsi-tst        not  numeric
                     move  zero           to   w-non-sta-sin
           else      move  w-tst-nsi-tst  to   w-non-sta-sin          .
           if        w-tst-nde-tst        not  numeric
                     move  zero           to   w-non-sta-des
           else      move  w-tst-nde-tst  to   w-non-sta-des          .
       let-tst-pss-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-tst-pss-999.
       let-tst-pss-999.
           exit.

      *    *===========================================================*
      *    * Open file [pss]                                           *
      *    *-----------------------------------------------------------*
       opn-fil-pss-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione error-code                      *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
      *              *-------------------------------------------------*
      *              * Preparazione pathname per [pss]                 *
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
           move      "pss"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-pss-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di Open                              *
      *              *-------------------------------------------------*
           open      i-o    pss                                       .
       opn-fil-pss-999.
           exit.

      *    *===========================================================*
      *    * Close file [pss]                                          *
      *    *-----------------------------------------------------------*
       cls-fil-pss-000.
           close      pss                                             .
       cls-fil-pss-999.
           exit.

      *    *===========================================================*
      *    * Ricerca e selezione di un parametro per il driver stampa  *
      *    *                                                           *
      *    * Input  : w-par-pds-par     = Parametro da ricercare       *
      *    *                                                           *
      *    * Output : w-par-pds-fdr     = Flag di esito sulla ricerca  *
      *    *                              - S : Parametro esistente    *
      *    *                              - N : Parametro non esisten- *
      *    *                                    te                     *
      *    *                                                           *
      *    *          w-par-pds-elx     = Elemento trovato             *
      *    *                                                           *
      *    *          w-par-pds-elx-lun = Elemento trovato, lunghezza  *
      *    *                                                           *
      *    *          w-par-pds-ely     = Elemento successivo a quello *
      *    *                              trovato                      *
      *    *                                                           *
      *    *          w-par-pds-ely-lun = Elemento successivo a quello *
      *    *                              trovato, lunghezza           *
      *    *-----------------------------------------------------------*
       ric-par-pds-000.
           move      "N"                  to   w-par-pds-fdr          .
           move      spaces               to   w-par-pds-elx          .
           move      zero                 to   w-par-pds-elx-lun      .
           move      spaces               to   w-par-pds-ely          .
           move      zero                 to   w-par-pds-ely-lun      .
       ric-par-pds-100.
           move      zero                 to   w-par-pds-inx          .
       ric-par-pds-200.
           add       1                    to   w-par-pds-inx          .
           if        w-par-pds-inx        >    w-par-pds-nel or
                     w-par-pds-inx        >    10
                     go to ric-par-pds-999.
           if        w-par-pds-ele
                    (w-par-pds-inx)       =    w-par-pds-par
                     go to ric-par-pds-300
           else      go to ric-par-pds-200.
       ric-par-pds-300.
           move      "S"                  to   w-par-pds-fdr          .
           move      w-par-pds-ele
                    (w-par-pds-inx)       to   w-par-pds-elx          .
           add       1                    to   w-par-pds-inx          .
           if        w-par-pds-inx        >    w-par-pds-nel or
                     w-par-pds-inx        >    10
                     go to ric-par-pds-400.
           move      w-par-pds-ele
                    (w-par-pds-inx)       to   w-par-pds-ely          .
       ric-par-pds-400.
           move      zero                 to   w-par-pds-ctr          .
           inspect   w-par-pds-elx    tallying w-par-pds-ctr
                                  for trailing spaces                 .
           move      05                   to   w-par-pds-elx-lun      .
           subtract  w-par-pds-ctr        from w-par-pds-elx-lun      .
       ric-par-pds-500.
           move      zero                 to   w-par-pds-ctr          .
           inspect   w-par-pds-ely    tallying w-par-pds-ctr
                                  for trailing spaces                 .
           move      05                   to   w-par-pds-ely-lun      .
           subtract  w-par-pds-ctr        from w-par-pds-ely-lun      .
       ric-par-pds-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo iniziali                  *
      *    *-----------------------------------------------------------*
       emi-seq-ini-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
           perform   emi-seq-ini-pre-000  thru emi-seq-ini-pre-999    .
       emi-seq-ini-050.
      *              *-------------------------------------------------*
      *              * Font di default                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Default                                     *
      *                  *---------------------------------------------*
           move      "Courier                      "
                                          to   w-par-def-fnt          .
      *                  *---------------------------------------------*
      *                  * Default per la scala                        *
      *                  *---------------------------------------------*
           move      "0.9"                to   w-par-def-sfn          .
      *                  *---------------------------------------------*
      *                  * Ricerca tra i parametri stampa associati al *
      *                  * tipo stampante, per vedere se esiste il pa- *
      *                  * rametro F=LGB. Se si : Si utilizza il font  *
      *                  * LetterGhotic-Bold come default, altrimenti  *
      *                  * si utilizza il Courier                      *
      *                  *---------------------------------------------*
           move      "F=LGB"              to   w-par-pds-par          .
           perform   ric-par-pds-000      thru ric-par-pds-999        .
           if        w-par-pds-fdr        =    "N"
                     go to emi-seq-ini-080.
      *                  *---------------------------------------------*
      *                  * LetterGothic-Bold                           *
      *                  *---------------------------------------------*
           move      "LetterGothic-Bold            "
                                          to   w-par-def-fnt          .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     emi-seq-ini-100.
       emi-seq-ini-080.
      *                  *---------------------------------------------*
      *                  * Ricerca tra i parametri stampa associati al *
      *                  * tipo stampante, per vedere se esiste il pa- *
      *                  * rametro F=LGB. Se si : Si utilizza il font  *
      *                  * LetterGhotic-Bold come default, altrimenti  *
      *                  * si utilizza il Courier                      *
      *                  *---------------------------------------------*
           move      "F=COB"              to   w-par-pds-par          .
           perform   ric-par-pds-000      thru ric-par-pds-999        .
           if        w-par-pds-fdr        =    "N"
                     go to emi-seq-ini-100.
      *                  *---------------------------------------------*
      *                  * LetterGothic-Bold                           *
      *                  *---------------------------------------------*
           move      "Courier-Bold                  "
                                          to   w-par-def-fnt          .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     emi-seq-ini-100.
       emi-seq-ini-100.
      *              *-------------------------------------------------*
      *              * 'Header Comments'                               *
      *              *-------------------------------------------------*
       emi-seq-ini-110.
      *                  *---------------------------------------------*
      *                  * 'PS-Adobe'                                  *
      *                  *                                             *
      *                  *  - PS-Adobe-1.0                             *
      *                  *---------------------------------------------*
           move      "%!PS-Adobe-1.0"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-120.
      *                  *---------------------------------------------*
      *                  * 'Title'                                     *
      *                  *                                             *
      *                  *  - Sistema applicativo                      *
      *                  *  - Area gestionale                          *
      *                  *  - Settore gestionale                       *
      *                  *  - Fase gestionale                          *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "%%Title: "
                                delimited by   size
                     p-sel-sis-app
                                delimited by   spaces
                     "-"        delimited by   size
                     p-sel-are-ges
                                delimited by   spaces
                     "-"        delimited by   size
                     p-sel-set-ges
                                delimited by   spaces
                     "-"        delimited by   size
                     p-sel-fas-ges
                                delimited by   spaces
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-125.
      *                  *---------------------------------------------*
      *                  * 'Copyright'                                 *
      *                  *---------------------------------------------*
           move      "%%Copyright: WIP - Sistemi Informativi Aziendali -
      -              " Padova - Italia"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-130.
      *                  *---------------------------------------------*
      *                  * 'Creator'                                   *
      *                  *                                             *
      *                  *  - Codice utente                            *
      *                  *  - Codice terminale                         *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "%%Creator: "
                                delimited by   size
                     p-sel-cod-ute
                                delimited by   spaces
                     "-"        delimited by   size
                     p-sel-cod-ter
                                delimited by   spaces
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-140.
      *                  *---------------------------------------------*
      *                  * 'CreationDate'                              *
      *                  *                                             *
      *                  *  - Data gg-mm-aaaa                          *
      *                  *  - Ora  hh:mm                               *
      *                  *---------------------------------------------*
           move      p-sel-dat-tim        to   w-sdt                  .
           move      w-sdt-mes            to   w-crd-mmm              .
           move      w-sdt-gio            to   w-crd-ddd              .
           move      w-sdt-ann            to   w-crd-yyy              .
           add       1900                 to   w-crd-yyy              .
           move      w-sdt-ora            to   w-crd-hhh              .
           move      w-sdt-min            to   w-crd-min              .
           move      spaces               to   stp-rec                .
           string    "%%CreationDate: "
                                delimited by   size
                     w-crd
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-150.
      *                  *---------------------------------------------*
      *                  * 'For'                                       *
      *                  *                                             *
      *                  *  - Codice azienda                           *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "%%For: "
                                delimited by   size
                     p-sel-cod-azi
                                delimited by   spaces
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-160.
      *                  *---------------------------------------------*
      *                  * 'Pages'                                     *
      *                  *                                             *
      *                  *  - atend                                    *
      *                  *---------------------------------------------*
           move      "%%Pages: (atend)"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-170.
      *                  *---------------------------------------------*
      *                  * 'BoundingBox'                               *
      *                  *                                             *
      *                  *  - 0                                        *
      *                  *  - 0                                        *
      *                  *  - Larghezza tipo stampante                 *
      *                  *  - Altezza tipo stampante                   *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "%%BoundingBox: 0 0 "
                                delimited by   size
                     w-lar-stp-int
                                delimited by   size
                     " "
                                delimited by   size
                     w-alt-stp-int
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-180.
      *                  *---------------------------------------------*
      *                  * 'DocumentFonts'                             *
      *                  *                                             *
      *                  * (da referenza)                              *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "%%DocumentFonts: "
                                delimited by   size
                     w-par-def-fnt
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-190.
      *                  *---------------------------------------------*
      *                  * 'EndComments'                               *
      *                  *---------------------------------------------*
           move      "%%EndComments"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-200.
      *              *-------------------------------------------------*
      *              * 'Prolog'                                        *
      *              *-------------------------------------------------*
       emi-seq-ini-205.
      *              *-------------------------------------------------*
      *              * Save Graphic Context                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ricerca tra i parametri stampa associati al *
      *                  * tipo stampante, per vedere se esiste il pa- *
      *                  * rametro S+R=N. Se si : non si emette la se- *
      *                  * sequenza di Save Graphic Context            *
      *                  *---------------------------------------------*
           move      "S+R=N"              to   w-par-pds-par          .
           perform   ric-par-pds-000      thru ric-par-pds-999        .
           if        w-par-pds-fdr        =    "S"
                     go to emi-seq-ini-215.
      *                  *---------------------------------------------*
      *                  * Emissione della sequenza vera e propria     *
      *                  *---------------------------------------------*
           move      "gsave"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-215.
      *                  *---------------------------------------------*
      *                  * Definizione della stringa che deve essere   *
      *                  * posta di fianco alla cornice, cosi' compo-  *
      *                  * sta :                                       *
      *                  *                                             *
      *                  *  - Literal per il nome del sistema informa- *
      *                  *    tivo Tangram e per il nome della societa'*
      *                  *    produttrice                              *
      *                  *                                             *
      *                  *  - Literal per i parametri di identifica-   *
      *                  *    zione                                    *
      *                  *                                             *
      *                  *  - Parametri di identificazione, separati   *
      *                  *    da un trattino                           *
      *                  *                                             *
      *                  *      - Sigla fase gestionale                *
      *                  *      - Codice azienda                       *
      *                  *      - Date e ora nel formato aaammgghhmm   *
      *                  *        con le cifre esposte specularmente   *
      *                  *      - Codice utente                        *
      *                  *      - Codice terminale                     *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
      *
           move      p-sel-dat-tim        to   w-sdt                  .
      *
           string    "/Tangram "
                                delimited by   size
                     "("
                                delimited by   size
                     "Sistema Informativo Tangram - WIP - PD"
                                delimited by   size
                     " - Identificazioni:"
                                delimited by   size
                     p-sel-fas-ges
                                delimited by   spaces
                     "-"
                                delimited by   size
                     p-sel-cod-azi
                                delimited by   spaces
                     "-"
                                delimited by   size
                     w-sdt-xxx (15)
                                delimited by   size
                     w-sdt-xxx (14)
                                delimited by   size
                     w-sdt-xxx (13)
                                delimited by   size
                     w-sdt-xxx (12)
                                delimited by   size
                     w-sdt-xxx (11)
                                delimited by   size
                     w-sdt-xxx (10)
                                delimited by   size
                     w-sdt-xxx (09)
                                delimited by   size
                     w-sdt-xxx (08)
                                delimited by   size
                     w-sdt-xxx (07)
                                delimited by   size
                     w-sdt-xxx (06)
                                delimited by   size
                     w-sdt-xxx (05)
                                delimited by   size
                     w-sdt-xxx (04)
                                delimited by   size
                     w-sdt-xxx (03)
                                delimited by   size
                     w-sdt-xxx (02)
                                delimited by   size
                     w-sdt-xxx (01)
                                delimited by   size
                     "-"
                                delimited by   size
                     p-sel-cod-ute
                                delimited by   spaces
                     "-"
                                delimited by   size
                     p-sel-cod-ter
                                delimited by   spaces
                     ")"
                                delimited by   size
                     " def"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-220.
      *                  *---------------------------------------------*
      *                  * Ampiezza della linea di stampa in caratte-  *
      *                  * ri, come determinato dal modulo 'mpslct'    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se per il modulo EPS e' stato passato   *
      *                      * come parametro, si pone quello          *
      *                      *-----------------------------------------*
           if        w-par-eps-wrk-als    =    spaces
                     go to emi-seq-ini-225.
           move      spaces               to   stp-rec                .
           string    "/AlsSel "
                                delimited by   size
                     w-par-eps-wrk-als
                                delimited by   spaces
                     " def"     delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
           go to     emi-seq-ini-230.
       emi-seq-ini-225.
      *                      *-----------------------------------------*
      *                      * Valore standard                         *
      *                      *-----------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/AlsSel "
                                delimited by   size
                     p-sel-als-sel
                                delimited by   size
                     " def"     delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-230.
      *                  *---------------------------------------------*
      *                  * Top effettivo in linee di stampa, come de-  *
      *                  * terminato dal modulo 'mpslct', il quale pe- *
      *                  * ro' lo pone al valore fisso di zero per     *
      *                  * tutte le stampanti di tipo PostScript, a    *
      *                  * meno che non sia stata scelta la forzatura  *
      *                  * con il carattere '*' alla posizione 99 del  *
      *                  * campo r-fix-esp-fut.                        *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/EftSel "
                                delimited by   size
                     p-sel-eft-sel
                                delimited by   size
                     " def"     delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-232.
      *                  *---------------------------------------------*
      *                  * Corpo effettivo in linee di stampa, come    *
      *                  * determinato dal modulo 'mpslct', il quale   *
      *                  * pero' lo pone al valore fisso di 72 per     *
      *                  * tutte le stampanti di tipo PostScript, a    *
      *                  * meno che non sia stata scelta la forzatura  *
      *                  * con il carattere '*' alla posizione 99 del  *
      *                  * campo r-fix-esp-fut.                        *
      *                  * N.B. : Per le PostScript di tipo A3, A2, A1 *
      *                  *        e A0, viene posto pari a 96          *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/EfcSel "
                                delimited by   size
                     p-sel-efc-sel
                                delimited by   size
                     " def"     delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-234.
      *                  *---------------------------------------------*
      *                  * Bottom effettivo in linee di stampa, come   *
      *                  * determinato dal modulo 'mpslct', il quale   *
      *                  * pero' lo pone al valore fisso di zero per   *
      *                  * tutte le stampanti di tipo PostScript, a    *
      *                  * meno che non sia stata scelta la forzatura  *
      *                  * con il carattere '*' alla posizione 99 del  *
      *                  * campo r-fix-esp-fut.                        *
      *                  * N.B. : Per le PostScript di tipo A3, A2, A1 *
      *                  *        e A0, il Top e il Bottom non vengono *
      *                  *        considerati                          *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/EfbSel "
                                delimited by   size
                     p-sel-efb-sel
                                delimited by   size
                     " def"     delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-236.
      *                  *---------------------------------------------*
      *                  * Contatore linee di stampa, come somma del   *
      *                  * Top, del Corpo, e del Bottom selezionati    *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           move      p-sel-eft-sel        to   w-set-nrl              .
           add       p-sel-efc-sel        to   w-set-nrl              .
           add       p-sel-efb-sel        to   w-set-nrl              .
           string    "/CtrSel "
                                delimited by   size
                     w-set-nrl
                                delimited by   size
                     " def"     delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-240.
      *                  *---------------------------------------------*
      *                  * Altezza per il tipo stampante, valore fis-  *
      *                  * so relativo al modulo Uni A4                *
      *                  *---------------------------------------------*
           move      w-alt-stp            to   w-set-4v4              .
      *
           move      spaces               to   stp-rec                .
           string    "/TstAlt "
                                delimited by   size
                     w-set-4v4
                                delimited by   size
                     " def"     delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-250.
      *                  *---------------------------------------------*
      *                  * Larghezza per il tipo stampante, valore     *
      *                  * fisso relativo al modulo Uni A4             *
      *                  *---------------------------------------------*
           move      w-lar-stp            to   w-set-4v4              .
           move      spaces               to   stp-rec                .
           string    "/TstLar "
                                delimited by   size
                     w-set-4v4
                                delimited by   size
                     " def"     delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-260.
      *                  *---------------------------------------------*
      *                  * Margine superiore per il tipo stampante     *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/TstMsu "
                                delimited by   size
                     w-set-msu
                                delimited by   size
                     " def"     delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-270.
      *                  *---------------------------------------------*
      *                  * Margine inferiore per il tipo stampante     *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/TstMin "
                                delimited by   size
                     w-set-min
                                delimited by   size
                     " def"     delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-280.
      *                  *---------------------------------------------*
      *                  * Margine sinistro per il tipo stampante      *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/TstMsi "
                                delimited by   size
                     w-set-msi
                                delimited by   size
                     " def"     delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-290.
      *                  *---------------------------------------------*
      *                  * Margine destro per il tipo stampante        *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/TstMde "
                                delimited by   size
                     w-set-mde
                                delimited by   size
                     " def"     delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-300.
      *                  *---------------------------------------------*
      *                  * Larghezza utile della pagina, come diffe-   *
      *                  * renza tra la larghezza per il tipo stam-    *
      *                  * pante ed i margini sinistro e destro        *
      *                  *---------------------------------------------*
           move      "/IpaWdt TstLar TstMde sub TstMsi sub def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-310.
      *                  *---------------------------------------------*
      *                  * Altezza utile della pagina, come differenza *
      *                  * tra l'altezza per il tipo stampante ed i    *
      *                  * margini superiore ed inferiore              *
      *                  *---------------------------------------------*
           move      "/IpaHgt TstAlt TstMsu sub TstMin sub def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-320.
      *                  *---------------------------------------------*
      *                  * Numero di linee per pagina : pari al numero *
      *                  * di linee stampa determinato da 'mpslct' au- *
      *                  * mentato di 1, quindi 72 + 1 = 73            *
      *                  * N.B. : Per le PostScript di tipo A3, A2, A1 *
      *                  *        e A0, viene posto pari a 96 + 1 = 97 *
      *                  *---------------------------------------------*
           move      "/IpaLns CtrSel 1 add def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-330.
      *                  *---------------------------------------------*
      *                  * Altezza per ogni linea : pari all'altezza   *
      *                  * utile della pagina divisa per il numero di  *
      *                  * linee per pagina                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se modulo 'EPS'                    *
      *                      *-----------------------------------------*
           if        p-sel-tip-mod        not  = "P"
                     go to emi-seq-ini-350.
       emi-seq-ini-335.
      *                      *-----------------------------------------*
      *                      * Test se passato come parametro          *
      *                      *                                         *
      *                      * N.B.: si estraggono i parametri dalla   *
      *                      *       area per le espansioni speciali   *
      *                      *                                         *
      *                      * Parametri contenuti:                    *
      *                      *                                         *
      *                      *  - Interlinea   : 99.99                 *
      *                      *  - Scala        : 9.9                   *
      *                      *  - Correttivo X : 99                    *
      *                      *  - Correttivo Y : 99                    *
      *                      *                                         *
      *                      *-----------------------------------------*
           if        w-par-eps-wrk-int    =    spaces
                     go to emi-seq-ini-350.
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/IpaHpl "
                                delimited by   size
                     w-par-eps-wrk-int
                                delimited by   size
                     " def"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     emi-seq-ini-400.
       emi-seq-ini-350.
      *                      *-----------------------------------------*
      *                      * Modulo normale                          *
      *                      *-----------------------------------------*
           move      "/IpaHpl IpaHgt IpaLns div def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-400.
      *                  *---------------------------------------------*
      *                  * Definizione eventuali parametri per file    *
      *                  * 'EPS'                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se necessario                      *
      *                      *-----------------------------------------*
           if        p-sel-tip-mod        not  = "P"
                     go to emi-seq-ini-500.
      *                      *-----------------------------------------*
      *                      * Assemblaggio Scala                      *
      *                      *-----------------------------------------*
           if        w-par-eps-wrk-scl    =    spaces
                     move  "001"          to   w-par-eps-wrk-scl      .
      *
           move      spaces               to   stp-rec                .
           string    "/EpsScl "
                                delimited by   size
                     w-par-eps-wrk-scl
                                delimited by   size
                     " def"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Assemblaggio Correttivo X               *
      *                      *-----------------------------------------*
           if        w-par-eps-wrk-crx    =    spaces
                     move  "00"           to   w-par-eps-wrk-crx      .
      *
           move      spaces               to   stp-rec                .
           string    "/EpsTsx "
                                delimited by   size
                     w-par-eps-wrk-crx
                                delimited by   size
                     " def"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Assemblaggio Correttivo Y               *
      *                      *-----------------------------------------*
           if        w-par-eps-wrk-cry    =    spaces
                     move  "00"           to   w-par-eps-wrk-cry      .
      *
           move      spaces               to   stp-rec                .
           string    "/EpsTsy "
                                delimited by   size
                     w-par-eps-wrk-cry
                                delimited by   size
                     " def"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-500.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'getc1'               *
      *                  *                                             *
      *                  * Presume di avere nello stack la prima linea *
      *                  * dell'array pagina, e ritorna nello stack    *
      *                  * il primo carattere di essa, oppure una      *
      *                  * stringa nulla                               *
      *                  *---------------------------------------------*
           move      "/getc1 {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "dup length 0 eq"    to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "{pop ()} {0 1 getinterval} ifelse"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-520.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'gotol'               *
      *                  *                                             *
      *                  * Posiziona il current-point all'inizio della *
      *                  * linea passata nello stack                   *
      *                  *---------------------------------------------*
           move      "/gotol {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Se il modulo e' di tipo EPS, il para-   *
      *                      * metro dell'interlinea e' assoggettato   *
      *                      * alla eventuale scala                    *
      *                      *-----------------------------------------*
           if        p-sel-tip-mod        not  = "P"
                     move  "IpaHpl mul IpaHgt exch sub 0 exch moveto"
                                          to   stp-rec
           else      move  "IpaHpl mul EpsScl mul IpaHgt exch sub 0 exch
      -              " moveto"            to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-530.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'prntl'               *
      *                  *                                             *
      *                  * Stampa la linea passata nello stack al nu-  *
      *                  * mero linea passato nello stack              *
      *                  *---------------------------------------------*
           move      "/prntl {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "gsave"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "gotol"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0 setgray"          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "show"               to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "grestore"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-540.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'reprn'               *
      *                  *                                             *
      *                  * Stampa la linea passata nello stack al nu-  *
      *                  * mero linea passato nello stack, pero' spo-  *
      *                  * stata di qualche pixel, in modo da simula-  *
      *                  * re l'effetto 'bold'                         *
      *                  *---------------------------------------------*
           move      "/reprn {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "gsave"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "gotol"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0.3 0.3 rmoveto"    to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0 setgray"          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "show"               to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "grestore"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-550.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'strkd'               *
      *                  *                                             *
      *                  * Stampa una linea continua scura per il 50%, *
      *                  * alta la meta' dell'altezza di una linea     *
      *                  *---------------------------------------------*
           move      "/strkd {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "gsave"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "newpath"            to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "gotol"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0.50 setgray"       to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IpaHpl 2 div setlinewidth"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0 IpaHpl 2 div rmoveto"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IpaWdt 0 rlineto"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "stroke"             to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "grestore"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-560.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'strks'               *
      *                  *                                             *
      *                  * Stampa una linea continua scura per il 25%, *
      *                  * alta un quarto' dell'altezza di una linea   *
      *                  *---------------------------------------------*
           move      "/strks {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "gsave"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "newpath"            to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "gotol"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0.75 setgray"       to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IpaHpl 4 div setlinewidth"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0 IpaHpl 3 div rmoveto"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IpaWdt 0 rlineto"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "stroke"             to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "grestore"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-565.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'strkf'               *
      *                  *                                             *
      *                  * Stampa una linea continua scura per il 10%, *
      *                  * dell'altezza di una linea, effetto lettura  *
      *                  * facilitata                                  *
      *                  *---------------------------------------------*
           move      "/strkf {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "gsave"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "newpath"            to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "gotol"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0.90 setgray"       to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IpaHpl setlinewidth"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0 IpaHpl 3 div rmoveto"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IpaWdt 0 rlineto"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "stroke"             to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "grestore"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-570.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'eject'               *
      *                  *                                             *
      *                  * Se il segnale 'outl?' e' Vero esegue la ri- *
      *                  * quadratura della pagina. In ogni caso poi   * 
      *                  * esegue lo showpage finale per la pagina.    *
      *                  *                                             *
      *                  * Nota : la riquadratura viene sempre esegui- *
      *                  *        ta con la matrice di trasformazione  *
      *                  *        relativa all'orientamento Portrait.  *
      *                  *---------------------------------------------*
           move      "/eject {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "gsave"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "outl? {"            to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "TrmDef setmatrix"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "newpath"            to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0 setgray"          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0.01 setlinewidth"  to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "-2 -2 moveto"       to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IpaWdt 2 add -2 lineto"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IpaWdt 2 add IpaHgt 2 add lineto"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "-2 IpaHgt 2 add lineto"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "closepath"          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "stroke"             to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           if        p-sel-als-sel        >    w-lim-rot
                     move  "IpaWdt -6 moveto 180 rotate"
                                          to   stp-rec
           else      move  "-6 0 moveto 90 rotate"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Font da referenza                       *
      *                      *-----------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/"
                                delimited by   size
                     w-par-def-fnt
                                delimited by   spaces
                     " findfont 4 scalefont setfont"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "Tangram show"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "OurFnt setfont"     to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "TrmUse setmatrix } if"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "showpage"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "grestore"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-580.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'readl'               *
      *                  *                                             *
      *                  * Lettura di una linea, e sua memorizzazione  *
      *                  * nell'array 'Pagina' secondo l'indice sul-   * 
      *                  * l'array 'InxLin', con incremento dell'in-   * 
      *                  * dice 'InxLin'.                              *
      *                  *---------------------------------------------*
           move      "/readl {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "Pagina"             to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "InxLin"             to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "currentfile 240 string readline pop"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "put"                to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/InxLin InxLin 1 add def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-590.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'readp'               *
      *                  *                                             *
      *                  * Lettura di una intera pagina, e sua memo-   *
      *                  * rizzazione nell'array 'Pagina'              * 
      *                  *---------------------------------------------*
           move      "/readp {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/Pagina 96 array def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/InxLin 0 def"      to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "CtrSel {readl} repeat"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-610.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'show1'               *
      *                  *                                             *
      *                  * Stampa della linea di separazione corri-    *
      *                  * spondente alla linea iniziale di '='.       *
      *                  * Inoltre il segnale 'IsSh1' viene posto al   *
      *                  * valore Falso, ed il segnale 'IsShD' viene   *
      *                  * posto al valore Vero                        *
      *                  *---------------------------------------------*
           move      "/show1 {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "NumLin strkd"       to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/IsSh1 false def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/IsShD true  def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-620.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'showd'               *
      *                  *                                             *
      *                  * Stampa di una linea intermedia tra la linea *
      *                  * iniziale di '=' e quella finale di '-', con *
      *                  * simulazione dell'effetto 'bold'.            *
      *                  * Inoltre, solo se la linea seguente corri-   *
      *                  * sponde alla linea finale di '-',  il segna- *
      *                  * le 'IsShD' viene posto al valore Falso, ed  *
      *                  * il segnale 'IsSh3' viene posto al valore    *
      *                  * Vero                                        *
      *                  *---------------------------------------------*
           move      "/showd {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "Savel NumLin prntl" to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "Savel NumLin reprn" to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Aggiunto un test di confronto anche per *
      *                      * un eventuale carattere '+' che si puo'  *
      *                      * trovare nelle stampe di hard-copy dello *
      *                      * screen                                  *
      *                      *-----------------------------------------*
           move      "Pagina NumLin get getc1 (-) eq Pagina NumLin get g
      -              "etc1 (+) eq or"     to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "{/IsShD false def /IsSh3 true def} if"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-630.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'show3'               *
      *                  *                                             *
      *                  * Stampa della linea di separazione corri-    *
      *                  * spondente alla linea finale di '-'.         *
      *                  * Inoltre il segnale 'IsSh3' viene posto al   *
      *                  * valore Falso, ed il segnale 'IsShN' viene   *
      *                  * posto al valore Vero                        *
      *                  *---------------------------------------------*
           move      "/show3 {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "NumLin strks"       to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/IsSh3 false def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/IsShN true  def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-640.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'shown'               *
      *                  *                                             *
      *                  * Stampa di una linea normale                 *
      *                  *---------------------------------------------*
           move      "/shown {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "Savel NumLin prntl" to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-645.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'showf'               *
      *                  *                                             *
      *                  * Stampa di una linea con lettura facilitata  *
      *                  *---------------------------------------------*
           move      "/showf {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "NumLin strkf"       to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-650.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'showl'               *
      *                  *                                             *
      *                  * Stampa di una linea dell' array 'Pagina' a  *
      *                  * seconda dell'indice linea 'InxLin', consi-  *
      *                  * derando se si deve stampare con :           *
      *                  *  - show1                                    *
      *                  *  - showd                                    *
      *                  *  - show3                                    *
      *                  *  - shown                                    *
      *                  *  - showf (lettura facilitata)               *
      *                  * a seconda del valore dei flags in On.       *
      *                  *---------------------------------------------*
           move      "/showl {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/NumLin InxLin 1 add def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Lettura facilitata                      *
      *                      *                                         *
      *                      * Le istruzioni per la lettura facilitata *
      *                      * vengono eseguite in base al tipo modulo *
      *                      *-----------------------------------------*
           if        p-sel-tip-mod        not  = "S"
                     go to emi-seq-ini-652.
      *
           move      "IsShF {/IsShF false def}"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "{/IsShF true def} ifelse"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "NumLin 6 lt"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "{/IsShF false def} if"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IsShF {showf} if"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Lettura facilitata                      *
      *                      *                                         *
      *                      * Fine gruppo di istruzioni               *
      *                      *-----------------------------------------*
       emi-seq-ini-652.
      *
           move      "/Savel Pagina InxLin get def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IsShN {shown} if"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IsSh3 {show3} if"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IsShD {showd} if"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IsSh1 {show1} if"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/InxLin NumLin def" to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-660.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'saltr'               *
      *                  *                                             *
      *                  * Stampa dell'intera pagina sapendo che deve  *
      *                  * essere considerata con linea iniziale di    *
      *                  * '=' e linea finale di '-'.                  *
      *                  *---------------------------------------------*
           move      "/saltr {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Se il modulo e' di tipo EPS, comunque   *
      *                      * non si deve eseguire la riquadratura    *
      *                      * a meno che non si tratti di un modulo   *
      *                      * con un barcode EPS all'interno          *
      *                      *-----------------------------------------*
           if        p-sel-tip-mod        not  = "P"
                     move  "/outl? true def"
                                          to   stp-rec
           else if   w-par-eps-spc-flg    =    "#"
                     move  "/outl? true def"
                                          to   stp-rec
           else      move  "/outl? false def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/InxLin 0 def"      to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/IsShN false def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/IsSh3 false def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/IsShD false def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/IsSh1 true  def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Lettura facilitata                      *
      *                      *                                         *
      *                      * Le istruzioni per la lettura facilitata *
      *                      * vengono eseguite in base al tipo modulo *
      *                      *-----------------------------------------*
           if        p-sel-tip-mod        not  = "S"
                     go to emi-seq-ini-665.
      *
           move      "/IsShF true  def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-665.
           move      "CtrSel {showl} repeat"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "eject"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-670.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'showa'               *
      *                  *                                             *
      *                  * Stampa di una linea dell'array 'Pagina' a   *
      *                  * seconda dell'indice linea 'InxLin', richia- *
      *                  * mando comunque :                            *
      *                  *  - shown                                    *
      *                  *---------------------------------------------*
           move      "/showa {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/NumLin InxLin 1 add def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/Savel Pagina InxLin get def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "shown"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/InxLin NumLin def" to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-680.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'sasci'               *
      *                  *                                             *
      *                  * Stampa dell'intera pagina sapendo che deve  *
      *                  * essere considerata senza la linea iniziale  *
      *                  * di '=' e senza la linea finale di '-'.      *
      *                  *---------------------------------------------*
           move      "/sasci {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/outl? false def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/InxLin 0 def"      to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "CtrSel {showa} repeat"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "eject"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-710.
      *                  *---------------------------------------------*
      *                  * Definizione procedura 'showp'               *
      *                  *                                             *
      *                  * Controlla la prima linea dell'array 'Pagi-  *
      *                  * na', e decide se la pagina da stampare de-  *
      *                  * ve essere considerata con la linea inizia-  *
      *                  * le di '=' e la linea finale di '-', oppure  *
      *                  * no. Di conseguenza richiama la subroutine   *
      *                  * di stampa pagina piu' adatta :              *
      *                  *   - sasci : per stampa normale              *
      *                  *   - saltr : per stampa con linea iniziale e *
      *                  *             finale                          *
      *                  *---------------------------------------------*
           move      "/showp {"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "Pagina 0 get getc1 (=) eq"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                  *---------------------------------------------*
      *                  * Ricerca tra i parametri stampa associati al *
      *                  * tipo stampante, per vedere se esiste il pa- *
      *                  * rametro L=NNN che esclude la cornice dalla  *
      *                  * pagina                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           move      "L=NNN"              to   w-par-pds-par          .
           perform   ric-par-pds-000      thru ric-par-pds-999        .
           if        w-par-pds-fdr        =    "N"
                     go to emi-seq-ini-730.
       emi-seq-ini-720.
      *                      *-----------------------------------------*
      *                      * Impaginazione senza cornice             *
      *                      *                                         *
      *                      * In pratica esegue in ogni caso la fase  *
      *                      * senza cornice                           *
      *                      *-----------------------------------------*
           move      "{sasci} {sasci} ifelse"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           go to     emi-seq-ini-750.
       emi-seq-ini-730.
      *                      *-----------------------------------------*
      *                      * Test standard                           *
      *                      *-----------------------------------------*
           move      "{saltr} {sasci} ifelse"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           go to     emi-seq-ini-750.
       emi-seq-ini-750.
           move      "} def"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-810.
      *                  *---------------------------------------------*
      *                  * Traslazione per fissare l'origine al punto  *
      *                  * lower-left in base ai margini               *
      *                  *---------------------------------------------*
           move      "TstMsi TstMin translate"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-820.
      *                  *---------------------------------------------*
      *                  * Salvataggio della 'Current Transformation   *
      *                  * Matrix, relativa all'orientamento 'Portrait'*
      *                  *---------------------------------------------*
           move      "/TrmDef matrix currentmatrix def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-830.
      *                  *---------------------------------------------*
      *                  * Test se modulo EPS con rotazione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-par-eps-wrk-rot    =    "000"  or
                     w-par-eps-wrk-rot    =    spaces
                     go to emi-seq-ini-831.
      *                      *-----------------------------------------*
      *                      * Rotazione                               *
      *                      *-----------------------------------------*
           move      "90 rotate"          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Traslazione                             *
      *                      *-----------------------------------------*
           move      "0 IpaWdt -1 mul translate"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
                     
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     emi-seq-ini-840.
       emi-seq-ini-831.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se superato il limite  *
      *                  * di rotazione oppure no                      *
      *                  *---------------------------------------------*
           if        p-sel-als-sel        >    w-lim-rot
                     go to emi-seq-ini-834.
       emi-seq-ini-832.
      *                      *-----------------------------------------*
      *                      * Se limite di rotazione non superato     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * 'Transformation Matrix' in uso pari *
      *                          * a quella relativa all'orientamento  *
      *                          * Portrait                            *
      *                          *-------------------------------------*
           move      "/TrmUse TrmDef def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     emi-seq-ini-840.
       emi-seq-ini-834.
      *                      *-----------------------------------------*
      *                      * Se limite di rotazione superato         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Rotazione, traslazione, e scala     *
      *                          *-------------------------------------*
           move      "90 rotate"          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0 IpaWdt -1 mul translate"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "IpaHgt IpaWdt div IpaWdt IpaHgt div scale"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                          *-------------------------------------*
      *                          * 'Transformation Matrix' in uso pari *
      *                          * a quella relativa all'orientamento  *
      *                          * Landscape                           *
      *                          *-------------------------------------*
           move      "/TrmUse matrix currentmatrix def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     emi-seq-ini-840.
       emi-seq-ini-840.
      *                  *---------------------------------------------*
      *                  * Font scale : pari ai nove decimi dell'al-   *
      *                  * tezza per ogni linea, salvo parametri in    *
      *                  * chiamata del modulo                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ricerca tra i parametri stampa associa- *
      *                      * ti al tipo stampante, per vedere se     *
      *                      * esiste il parametro X=2.1 per aumentare *
      *                      * il Font Scale (ingrandimento del font). *
      *                      *-----------------------------------------*
           move      "X=2.1"              to   w-par-pds-par          .
           perform   ric-par-pds-000      thru ric-par-pds-999        .
           if        w-par-pds-fdr        =    "N"
                     go to emi-seq-ini-845.
      *                      *-----------------------------------------*
      *                      * Forzatura Font Scale                    *
      *                      *-----------------------------------------*
           move      "2.1"                to   w-par-def-sfn          .
       emi-seq-ini-845.
      *                      *-----------------------------------------*
      *                      * Test se passato come parametro          *
      *                      *-----------------------------------------*
           if        w-par-eps-wrk-sfn    not  = spaces
                     move  w-par-eps-wrk-sfn
                                          to   w-par-def-sfn          .
      *                      *-----------------------------------------*
      *                      * Composizione comando                    *
      *                      *-----------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/FntScl IpaHpl "
                                delimited by   size
                     w-par-def-sfn
                                delimited by   size
                     " mul def"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-850.
      *                  *---------------------------------------------*
      *                  * Definizione del font 'OurFnt' in funzione   *
      *                  * della 'Font scale' appena determinata, e    *
      *                  * tirato in orizzontale in modo da contenere  *
      *                  * perfettamente il numero di caratteri per    *
      *                  * linea di stampa                             *
      *                  *---------------------------------------------*
           move      "/FntHfc"            to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "gsave"              to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Font da referenza                       *
      *                      *-----------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/"
                                delimited by   size
                     w-par-def-fnt
                                delimited by   spaces
                     " findfont FntScl scalefont setfont"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "(m) stringwidth pop"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "AlsSel mul IpaWdt exch div"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "grestore"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                  *---------------------------------------------*
      *                  * Definizione del font                        *
      *                  *                                             *
      *                  * Fine                                        *
      *                  *---------------------------------------------*
           move      "def"                to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/FntMtx [FntScl FntHfc mul 0 0 FntScl 0 0] def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                      *-----------------------------------------*
      *                      * Font da referenza                       *
      *                      *-----------------------------------------*
           move      spaces               to   stp-rec                .
           string    "/OurFnt /"
                                delimited by   size
                     w-par-def-fnt
                                delimited by   spaces
                     " findfont FntMtx makefont def"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-860.
      *                  *---------------------------------------------*
      *                  * Set del font preparato OurFnt               *
      *                  *---------------------------------------------*
           move      "OurFnt setfont"     to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-870.
      *                  *---------------------------------------------*
      *                  * Definizione della procedura di inizio in-   *
      *                  * clusione di un file 'EPS' contenente il lo- *
      *                  * go grafico e/o il documento                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se necessario                      *
      *                      *                                         *
      *                      * Le istruzioni per l'inclusione di un    *
      *                      * file Postscript EPS vengono eseguite in *
      *                      * base al tipo modulo                     *
      *                      *-----------------------------------------*
           if        p-sel-tip-mod        not  = "P"
                     go to emi-seq-ini-900.
      *                      *-----------------------------------------*
      *                      * Preparazione                            *
      *                      *-----------------------------------------*
           move      "/BeginEPSF {/b4_Inc_state save def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/dict_count countdictstack def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/op_count count 1 sub def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "userdict begin"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/showpage {} def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "0 setgray 0 setlinecap"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "1 setlinewidth 0 setlinejoin"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "10 setmiterlimit [] 0 setdash newpath"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/languagelevel where"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "{pop languagelevel"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "1 ne"               to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "{false setstrokeadjust false setoverprint"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} if"               to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} if"               to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} bind def"         to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-880.
      *                  *---------------------------------------------*
      *                  * Definizione della procedura di fine inclu-  *
      *                  * sione di un file 'EPS' contenente il logo   *
      *                  * grafico e/o il documento                    *
      *                  *---------------------------------------------*
           move      "/EndEPSF {"         to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "count op_count sub {pop} repeat"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "countdictstack dict_count sub {end} repeat"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "b4_Inc_state restore"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "} bind def"         to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-900.
      *              *-------------------------------------------------*
      *              * 'EndProlog'                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Output End prolog                           *
      *                  *---------------------------------------------*
           move      "%%EndProlog"        to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-ini-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo iniziali                  *
      *    *                                                           *
      *    * Operazioni preliminari                                    *
      *    *-----------------------------------------------------------*
       emi-seq-ini-pre-000.
      *              *-------------------------------------------------*
      *              * Numero pagine nel documento PostScript : zero   *
      *              *-------------------------------------------------*
           move      zero                 to   w-nps                  .
       emi-seq-ini-pre-050.
      *              *-------------------------------------------------*
      *              * Preparazione costanti                           *
      *              *-------------------------------------------------*
       emi-seq-ini-pre-060.
      *                  *---------------------------------------------*
      *                  * Altezza e larghezza per il tipo stampante   *
      *                  *---------------------------------------------*
       emi-seq-ini-pre-062.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della sottoclasse di   *
      *                  * stampante PostScript                        *
      *                  *---------------------------------------------*
           if        w-par-scl-psc        =    "A0  "
                     go to emi-seq-ini-pre-070
           else if   w-par-scl-psc        =    "A1  "
                     go to emi-seq-ini-pre-071
           else if   w-par-scl-psc        =    "A2  "
                     go to emi-seq-ini-pre-072
           else if   w-par-scl-psc        =    "A3  "
                     go to emi-seq-ini-pre-073
           else      go to emi-seq-ini-pre-074.
       emi-seq-ini-pre-070.
      *                  *---------------------------------------------*
      *                  * Se sottoclasse PostScript : A0              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Altezza per il tipo stampante, parte    *
      *                      * intera e parte decimale                 *
      *                      *-----------------------------------------*
           move      3367                 to   w-alt-stp-int          .
           move      5592                 to   w-alt-stp-dec          .
      *                      *-----------------------------------------*
      *                      * Larghezza per il tipo stampante, parte  *
      *                      * intera e parte decimale                 *
      *                      *-----------------------------------------*
           move      2381                 to   w-lar-stp-int          .
           move      1024                 to   w-lar-stp-dec          .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     emi-seq-ini-pre-080.
       emi-seq-ini-pre-071.
      *                  *---------------------------------------------*
      *                  * Se sottoclasse PostScript : A1              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Altezza per il tipo stampante, parte    *
      *                      * intera e parte decimale                 *
      *                      *-----------------------------------------*
           move      2381                 to   w-alt-stp-int          .
           move      1024                 to   w-alt-stp-dec          .
      *                      *-----------------------------------------*
      *                      * Larghezza per il tipo stampante, parte  *
      *                      * intera e parte decimale                 *
      *                      *-----------------------------------------*
           move      1683                 to   w-lar-stp-int          .
           move      7796                 to   w-lar-stp-dec          .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     emi-seq-ini-pre-080.
       emi-seq-ini-pre-072.
      *                  *---------------------------------------------*
      *                  * Se sottoclasse PostScript : A2              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Altezza per il tipo stampante, parte    *
      *                      * intera e parte decimale                 *
      *                      *-----------------------------------------*
           move      1683                 to   w-alt-stp-int          .
           move      7796                 to   w-alt-stp-dec          .
      *                      *-----------------------------------------*
      *                      * Larghezza per il tipo stampante, parte  *
      *                      * intera e parte decimale                 *
      *                      *-----------------------------------------*
           move      1190                 to   w-lar-stp-int          .
           move      5512                 to   w-lar-stp-dec          .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     emi-seq-ini-pre-080.
       emi-seq-ini-pre-073.
      *                  *---------------------------------------------*
      *                  * Se sottoclasse PostScript : A3              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Altezza per il tipo stampante, parte    *
      *                      * intera e parte decimale                 *
      *                      *-----------------------------------------*
           move      1190                 to   w-alt-stp-int          .
           move      5512                 to   w-alt-stp-dec          .
      *                      *-----------------------------------------*
      *                      * Larghezza per il tipo stampante, parte  *
      *                      * intera e parte decimale                 *
      *                      *-----------------------------------------*
           move      0841                 to   w-lar-stp-int          .
           move      8898                 to   w-lar-stp-dec          .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     emi-seq-ini-pre-080.
       emi-seq-ini-pre-074.
      *                  *---------------------------------------------*
      *                  * Se sottoclasse PostScript : A4              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Altezza per il tipo stampante, parte    *
      *                      * intera e parte decimale                 *
      *                      *-----------------------------------------*
           move      0841                 to   w-alt-stp-int          .
           move      8898                 to   w-alt-stp-dec          .
      *                      *-----------------------------------------*
      *                      * Larghezza per il tipo stampante, parte  *
      *                      * intera e parte decimale                 *
      *                      *-----------------------------------------*
           move      0595                 to   w-lar-stp-int          .
           move      2756                 to   w-lar-stp-dec          .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     emi-seq-ini-pre-080.
       emi-seq-ini-pre-080.
      *                  *---------------------------------------------*
      *                  * Limite di rotazione, superato il quale si   *
      *                  * deve eseguire una rotazione per passare     *
      *                  * dal formato 'Portrait' a 'Landscape' cioe'  *
      *                  * 'Verticale' o 'Orizzontale'                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Forzatura preliminare allo standard     *
      *                      *-----------------------------------------*
           move      132                  to   w-lim-rot              .
      *                      *-----------------------------------------*
      *                      * Ricerca tra i parametri stampa associa- *
      *                      * ti altipo stampante, per vedere se      *
      *                      * esiste il parametro O=xxx. In tal caso, *
      *                      * si forza il limite di rotazione per la  *
      *                      * rotazione obbligata richiesta (vertica- *
      *                      * le od orizzontale                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Controllo se orizzontale            *
      *                          *-------------------------------------*
           move      "O=ORI"              to   w-par-pds-par          .
           perform   ric-par-pds-000      thru ric-par-pds-999        .
           if        w-par-pds-fdr        =    "N"
                     go to emi-seq-ini-pre-090.
      *                          *-------------------------------------*
      *                          * Se orizzontale                      *
      *                          *-------------------------------------*
           move      000                  to   w-lim-rot              .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     emi-seq-ini-pre-100.
       emi-seq-ini-pre-090.
      *                          *-------------------------------------*
      *                          * Controllo se verticale              *
      *                          *-------------------------------------*
           move      "O=VER"              to   w-par-pds-par          .
           perform   ric-par-pds-000      thru ric-par-pds-999        .
           if        w-par-pds-fdr        =    "N"
                     go to emi-seq-ini-pre-100.
      *                          *-------------------------------------*
      *                          * Se verticale                        *
      *                          *-------------------------------------*
           move      999                  to   w-lim-rot              .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     emi-seq-ini-pre-100.
       emi-seq-ini-pre-100.
      *              *-------------------------------------------------*
      *              * Settaggio margini                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Margine superiore                           *
      *                  *---------------------------------------------*
           move      p-sel-msu-tst        to   w-set-msu              .
      *                  *---------------------------------------------*
      *                  * Margine inferiore                           *
      *                  *---------------------------------------------*
           move      p-sel-min-tst        to   w-set-min              .
      *                  *---------------------------------------------*
      *                  * Margine sinistro                            *
      *                  *---------------------------------------------*
           move      p-sel-msi-tst        to   w-set-msi              .
      *                  *---------------------------------------------*
      *                  * Margine destro                              *
      *                  *---------------------------------------------*
           move      p-sel-mde-tst        to   w-set-mde              .
       emi-seq-ini-pre-300.
      *              *-------------------------------------------------*
      *              * Lettura parametri passati                       *
      *              *-------------------------------------------------*
       emi-seq-ini-pre-310.
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-par-eps-wrk-int      .
           move      spaces               to   w-par-eps-wrk-scl      .
           move      zero                 to   w-par-eps-wrk-msu      .
           move      spaces               to   w-par-eps-wrk-min      .
           move      spaces               to   w-par-eps-wrk-msi      .
           move      spaces               to   w-par-eps-wrk-mde      .
           move      spaces               to   w-par-eps-wrk-crx      .
           move      spaces               to   w-par-eps-wrk-cry      .
           move      spaces               to   w-par-eps-wrk-rot      .
           move      spaces               to   w-par-eps-wrk-als      .
           move      zero                 to   w-par-eps-wrk-nlf      .
           move      zero                 to   w-par-eps-wrk-nel      .
           move      zero                 to   w-par-eps-wrk-cma      .
           move      zero                 to   w-par-eps-wrk-cli      .
           move      spaces               to   w-par-eps-wrk-pe1      .
           move      spaces               to   w-par-eps-wrk-se1      .
           move      spaces               to   w-par-eps-wrk-pe2      .
           move      spaces               to   w-par-eps-wrk-se2      .
           move      spaces               to   w-par-eps-wrk-pe3      .
           move      spaces               to   w-par-eps-wrk-se3      .
       emi-seq-ini-pre-320.
      *                  *---------------------------------------------*
      *                  * Eventuali parametri speciali per file 'eps' *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su primo carattere Area riservata  *
      *                      * per funzioni speciali                   *
      *                      *-----------------------------------------*
           if        p-sel-spc-sel
                    (01 : 01)             =    spaces
                     go to emi-seq-ini-pre-800.
      *                      *-----------------------------------------*
      *                      * Estrazione master flag                  *
      *                      *-----------------------------------------*
           move      p-sel-spc-sel        to   w-par-eps-spc          .
      *                      *-----------------------------------------*
      *                      * Controllo preliminare                   *
      *                      *-----------------------------------------*
           if        w-par-eps-spc-flg    not  = "@" and
                     w-par-eps-spc-flg    not  = "+" and
                     w-par-eps-spc-flg    not  = "#"
                     move  spaces         to   w-par-eps-spc
                     go to emi-seq-ini-pre-800.
       emi-seq-ini-pre-330.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del flag master      *
      *                  *---------------------------------------------*
           if        w-par-eps-spc-flg    = "@"
                     go to emi-seq-ini-pre-400
           else if   w-par-eps-spc-flg    = "+"
                     go to emi-seq-ini-pre-500
           else if   w-par-eps-spc-flg    = "#"
                     go to emi-seq-ini-pre-600
           else      go to emi-seq-ini-pre-800.
       emi-seq-ini-pre-400.
      *                  *---------------------------------------------*
      *                  * '@'                                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valori letti in campi di comodo         *
      *                      *-----------------------------------------*
           move      w-par-eps-spc-int    to   w-par-eps-wrk-int      .
           move      w-par-eps-spc-scl    to   w-par-eps-wrk-scl      .
           move      w-par-eps-spc-msu    to   w-par-eps-wrk-msu      .
           move      w-par-eps-spc-min    to   w-par-eps-wrk-min      .
           move      w-par-eps-spc-msi    to   w-par-eps-wrk-msi      .
           move      w-par-eps-spc-mde    to   w-par-eps-wrk-mde      .
           move      w-par-eps-spc-crx    to   w-par-eps-wrk-crx      .
           move      w-par-eps-spc-cry    to   w-par-eps-wrk-cry      .
           move      w-par-eps-spc-rot    to   w-par-eps-wrk-rot      .
           move      w-par-eps-spc-als    to   w-par-eps-wrk-als      .
      *                      *-----------------------------------------*
      *                      * Ritaratura margini                      *
      *                      *-----------------------------------------*
           move      w-par-eps-wrk-msu    to   w-set-msu              .
           move      w-par-eps-wrk-min    to   w-set-min              .
           move      w-par-eps-wrk-msi    to   w-set-msi              .
           move      w-par-eps-wrk-mde    to   w-set-mde              .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     emi-seq-ini-pre-800.
       emi-seq-ini-pre-500.
      *                  *---------------------------------------------*
      *                  * '+'                                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valori letti in campi di comodo         *
      *                      *-----------------------------------------*
           move      w-par-eps-spc-nlf    to   w-par-eps-wrk-nlf      .
           move      w-par-eps-spc-nel    to   w-par-eps-wrk-nel      .
           move      w-par-eps-spc-cma    to   w-par-eps-wrk-cma      .
           move      w-par-eps-spc-cli    to   w-par-eps-wrk-cli      .
           move      w-par-eps-spc-sfn    to   w-par-eps-wrk-sfn      .
           if        w-par-eps-wrk-sfn    =    spaces
                     move  "0.9"          to   w-par-eps-wrk-sfn      .
      *                      *-----------------------------------------*
      *                      * Fattore di incremento per EPS           *
      *                      *-----------------------------------------*
           move      w-alt-stp-int        to   w-set-fin              .
           subtract  w-set-msu            from w-set-fin              .
           subtract  w-set-min            from w-set-fin              .
           subtract  w-par-eps-wrk-cma    from w-set-fin              .
           divide    w-par-eps-wrk-nlf    into w-set-fin              .
      *                      *-----------------------------------------*
      *                      * Prima linea per EPS                     *
      *                      *-----------------------------------------*
           move      w-alt-stp-int        to   w-set-top              .
           subtract  w-set-msu            from w-set-top              .
           add       w-par-eps-wrk-cli    to   w-set-top              .
           subtract  w-par-eps-wrk-cma    from w-set-top              .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     emi-seq-ini-pre-800.
       emi-seq-ini-pre-600.
      *                  *---------------------------------------------*
      *                  * '#'                                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Per convenzione, in presenza di questo  *
      *                      * indicatore, si forza il 'tipo modulo'   *
      *                      * in modo che si comporti come nel caso   *
      *                      * dei moduli EPS                          *
      *                      *-----------------------------------------*
           move      "P"                  to   p-sel-tip-mod          .
      *                      *-----------------------------------------*
      *                      * Valori letti in campi di comodo         *
      *                      *-----------------------------------------*
           move      w-par-eps-spc-pag    to   w-par-eps-wrk-lfl      .
           move      w-par-eps-spc-lin    to   w-par-eps-wrk-lin      .
           move      w-par-eps-spc-pos    to   w-par-eps-wrk-pos      .
           move      w-par-eps-spc-lun    to   w-par-eps-wrk-lun      .
           move      w-par-eps-spc-xxx    to   w-par-eps-wrk-xxx      .
           move      w-par-eps-spc-yyy    to   w-par-eps-wrk-yyy      .
           move      w-par-eps-spc-sep    to   w-par-eps-wrk-sep      .
           move      w-par-eps-spc-sef    to   w-par-eps-wrk-sef      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     emi-seq-ini-pre-800.
       emi-seq-ini-pre-800.
      *              *-------------------------------------------------*
      *              * Normalizzazioni finali                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Margine superiore                           *
      *                  *---------------------------------------------*
           if        w-set-msu            =    zero
                     move  0050           to   w-set-msu              .
      *                  *---------------------------------------------*
      *                  * Margine inferiore                           *
      *                  *---------------------------------------------*
           if        w-set-min            =    zero
                     move  0050           to   w-set-min              .
      *                  *---------------------------------------------*
      *                  * Margine destro                              *
      *                  *---------------------------------------------*
           if        w-set-mde            =    zero
                     move  0050           to   w-set-mde              .
      *                  *---------------------------------------------*
      *                  * Margine sinistro                            *
      *                  *---------------------------------------------*
           if        w-set-msi            =    zero
                     move  0050           to   w-set-msi              .
       emi-seq-ini-pre-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-seq-ini-pre-999.
       emi-seq-ini-pre-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo finali                    *
      *    *-----------------------------------------------------------*
       emi-seq-fin-000.
      *              *-------------------------------------------------*
      *              * Restore Graphic Context                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ricerca tra i parametri stampa associati al *
      *                  * tipo stampante, per vedere se esiste il pa- *
      *                  * rametro S+R=N. Se si : non si emette la se- *
      *                  * sequenza di Restore Graphic Context         *
      *                  *---------------------------------------------*
           move      "S+R=N"              to   w-par-pds-par          .
           perform   ric-par-pds-000      thru ric-par-pds-999        .
           if        w-par-pds-fdr        =    "S"
                     go to emi-seq-fin-100.
      *                  *---------------------------------------------*
      *                  * Emissione della sequenza vera e propria     *
      *                  *---------------------------------------------*
           move      "grestore"           to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-fin-100.
      *              *-------------------------------------------------*
      *              * 'Trailer' comment                               *
      *              *-------------------------------------------------*
           move      "%%Trailer"          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-fin-200.
      *              *-------------------------------------------------*
      *              * 'Pages' comment                                 *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "%%Pages: "
                                delimited by   size
                     w-nps      
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-fin-300.
      *              *-------------------------------------------------*
      *              * End of Transmission                             *
      *              *-------------------------------------------------*
       emi-seq-fin-310.
      *                  *---------------------------------------------*
      *                  * Ricerca tra i parametri stampa associati al *
      *                  * tipo stampante, per vedere se esiste il pa- *
      *                  * rametro EOT=N. Se si' : non si emette la    *
      *                  * sequenza End Of Transmission.               *
      *                  *---------------------------------------------*
           move      "EOT=N"              to   w-par-pds-par          .
           perform   ric-par-pds-000      thru ric-par-pds-999        .
           if        w-par-pds-fdr        =    "S"
                     go to emi-seq-fin-400.
      *                  *---------------------------------------------*
      *                  * Emissione EOT vera e propria                *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-c0c-EOT
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-fin-400.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-seq-fin-999.
       emi-seq-fin-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo di inizio pagina          *
      *    *-----------------------------------------------------------*
       emi-seq-inp-000.
      *              *-------------------------------------------------*
      *              * Numero pagine nel documento PostScript : +1     *
      *              *-------------------------------------------------*
           add       1                    to   w-nps                  .
       emi-seq-inp-010.
      *              *-------------------------------------------------*
      *              * 'Page' comment                                  *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "%%Page: "
                                delimited by   size
                     w-nps      delimited by   size
                     " "        delimited by   size
                     w-nps      delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-inp-050.
      *              *-------------------------------------------------*
      *              * Eventuale inclusione EPS                        *
      *              *-------------------------------------------------*
           perform   emi-seq-inp-pre-000  thru emi-seq-inp-pre-999    .
       emi-seq-inp-800.
      *              *-------------------------------------------------*
      *              * Caricamento dell'array 'Pagina' con i contenuti *
      *              * delle linee della pagina                        *
      *              *-------------------------------------------------*
           move      "save readp"         to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-inp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-seq-inp-999.
       emi-seq-inp-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo di inizio pagina          *
      *    *                                                           *
      *    * Subroutine preliminare per inclusione EPS                 *
      *    *-----------------------------------------------------------*
       emi-seq-inp-pre-000.
      *              *-------------------------------------------------*
      *              * Test se inclusione Barcode EPS nel modulo da    *
      *              * preparare                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su parametro indicatore                *
      *                  *---------------------------------------------*
           if        w-par-eps-spc-flg    not  = "#"
                     go to emi-seq-inp-pre-400.
      *                  *---------------------------------------------*
      *                  * Test su parametro linea pagina              *
      *                  *---------------------------------------------*
           if        w-par-eps-wrk-lfl    =    zero
                     go to emi-seq-inp-pre-400.
       emi-seq-inp-pre-020.
      *              *-------------------------------------------------*
      *              * Bufferizzazione elementi                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura alla linea passata come parametro   *
      *                  * del primo carattere per rilevare eventuale  *
      *                  * flag                                        *
      *                  *---------------------------------------------*
           move      p-buf-lin
                    (w-par-eps-wrk-lfl)
                    (01 : 01)             to   w-par-eps-wrk-flg      .
      *                  *---------------------------------------------*
      *                  * Se flag non trovato                         *
      *                  *---------------------------------------------*
           if        w-par-eps-wrk-flg    =    spaces
                     go to emi-seq-inp-pre-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag                        *
      *                  *---------------------------------------------*
           move      spaces               to   w-par-eps-wrk-flg      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione area di estrazione          *
      *                  *---------------------------------------------*
           move      spaces               to   p-buf-lin
                                              (w-par-eps-wrk-lfl)
                                              (01 : 01)               .
______*    move      zero                 to   w-par-eps-wrk-lfl      .
      *                  *---------------------------------------------*
      *                  * Estrazione stringa alle coordinate previste *
      *                  * destinato a diventare barcode               *
      *                  *---------------------------------------------*
           move      p-buf-lin
                    (w-par-eps-spc-lin)
                    (w-par-eps-spc-pos : 
                     w-par-eps-spc-lun)   to   w-par-eps-wrk-num      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione area di estrazione          *
      *                  *---------------------------------------------*
           move      spaces               to   p-buf-lin
                                              (w-par-eps-spc-lin)
                                              (w-par-eps-spc-pos : 
                                               w-par-eps-spc-lun)     .
      *                  *---------------------------------------------*
      *                  * Test su numero estratto                     *
      *                  *---------------------------------------------*
           if        w-par-eps-wrk-num    =    spaces
                     go to emi-seq-inp-pre-400
           else      go to emi-seq-inp-pre-300.
       emi-seq-inp-pre-300.
      *                  *---------------------------------------------*
      *                  * Open modulo di determinazione bar-code      *
      *                  *---------------------------------------------*
           perform   det-bar-cod-opn-000  thru det-bar-cod-opn-999    .
      *                  *---------------------------------------------*
      *                  * Generazione barcode EPS                     *
      *                  *---------------------------------------------*
           perform   emi-seq-inp-gbc-000  thru emi-seq-inp-gbc-999    .
      *                  *---------------------------------------------*
      *                  * Inclusione EPS da buffer                    *
      *                  *---------------------------------------------*
           move      w-par-eps-wrk-xxx    to   w-buf-xxx-eps          .
           move      w-par-eps-wrk-yyy    to   w-buf-yyy-eps          .
           move      w-par-eps-wrk-sep    to   w-buf-scl-eps          .
           perform   emi-seq-inp-epb-000  thru emi-seq-inp-epb-999    .
      *                  *---------------------------------------------*
      *                  * Close modulo di determinazione bar-code     *
      *                  *---------------------------------------------*
           perform   det-bar-cod-cls-000  thru det-bar-cod-cls-999    .
      *                  *---------------------------------------------*
      *                  * Normalizzazione barcode                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-par-eps-wrk-num      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     emi-seq-inp-pre-400.
       emi-seq-inp-pre-400.
      *              *=================================================*
      *              * Test se modulo PostScript                       *
      *              *-------------------------------------------------*
           if        p-sel-tip-mod        not  = "P"
                     go to emi-seq-inp-pre-900.
      *              *-------------------------------------------------*
      *              * Test se necessario bufferizzare pathnames per   *
      *              * immagini EPS                                    *
      *              *-------------------------------------------------*
           if        w-par-eps-spc-flg    not  = "+"
                     go to emi-seq-inp-pre-800.
       emi-seq-inp-pre-500.
      *              *-------------------------------------------------*
      *              * Ciclo di bufferizzazione                        *
      *              *-------------------------------------------------*
           perform   emi-seq-inp-bpt-000  thru emi-seq-inp-bpt-999    .
       emi-seq-inp-pre-600.
      *              *-------------------------------------------------*
      *              * Ciclo di emissione                              *
      *              *-------------------------------------------------*
           perform   emi-seq-inp-ebf-000  thru emi-seq-inp-ebf-999    .
       emi-seq-inp-pre-800.
      *              *-------------------------------------------------*
      *              * Determinazione del pathname file 'eps'          *
      *              *-------------------------------------------------*
           move      spaces               to   w-pat-eps-nam          .
           string    "mod/"     delimited by   size
                     p-sel-mod-sel
                                delimited by   spaces
                                          into w-pat-eps-nam          .
           perform   emi-seq-inp-pts-000  thru emi-seq-inp-pts-999    .
      *              *-------------------------------------------------*
      *              * Inclusione EPS                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-xxx-eps          .
           move      zero                 to   w-buf-yyy-eps          .
           move      spaces               to   w-buf-scl-eps          .
           perform   emi-seq-inp-eps-000  thru emi-seq-inp-eps-999    .
       emi-seq-inp-pre-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-seq-inp-pre-999.
       emi-seq-inp-pre-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo di inizio pagina          *
      *    *                                                           *
      *    * Subroutine preliminare per generazione eventuale barcode  *
      *    *-----------------------------------------------------------*
       emi-seq-inp-gbc-000.
      *              *-------------------------------------------------*
      *              * Generazione barcode                             *
      *              *-------------------------------------------------*
           move      "AP"                 to   d-bar-cod-tip-ope      .
           move      "C128N06C"           to   d-bar-cod-tip-cod      .
           move      "A"                  to   d-bar-cod-tip-cdp      .
           move      w-par-eps-wrk-num    to   d-bar-cod-alf-cod      .
           perform   det-bar-cod-cll-000  thru det-bar-cod-cll-999    .
      *              *-------------------------------------------------*
      *              * Determinazione del chek digit                   *
      *              *-------------------------------------------------*
           move      "CD"                 to   d-bar-cod-tip-ope      .
           move      "C128N06C"           to   d-bar-cod-tip-cod      .
           perform   det-bar-cod-cll-000  thru det-bar-cod-cll-999    .
      *              *-------------------------------------------------*
      *              * Scrittura su buffer                             *
      *              *-------------------------------------------------*
           move      "MB"                 to   d-bar-cod-tip-ope      .
           move      "C128N06C"           to   d-bar-cod-tip-cod      .
           perform   det-bar-cod-cll-000  thru det-bar-cod-cll-999    .
       emi-seq-inp-gbc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-seq-inp-gbc-999.
       emi-seq-inp-gbc-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo di inizio pagina          *
      *    *                                                           *
      *    * Subroutine di bufferizzazione pathnames EPS               *
      *    *-----------------------------------------------------------*
       emi-seq-inp-bpt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-wrk-pt1          .
           move      spaces               to   w-buf-wrk-pt2          .
           move      spaces               to   w-buf-wrk-pt3          .
       emi-seq-inp-bpt-100.
      *              *-------------------------------------------------*
      *              * Ciclo 1..96                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatori                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-lin                  .
           move      zero                 to   w-buf-num-ele          .
       emi-seq-inp-bpt-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-lin                  .
      *                  *---------------------------------------------*
      *                  * Test se fine Pagina                         *
      *                  *---------------------------------------------*
           if        w-lin                >    72
                     go to emi-seq-inp-bpt-900.
      *                  *---------------------------------------------*
      *                  * Test se elemento da estrarre                *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-ctr-001          .
           inspect   p-buf-lin (w-lin)
                                      tallying w-buf-ctr-001
                                          for  all "{"                .
           if        w-buf-ctr-001        =    zero or
                     w-buf-ctr-001        >    1
                     go to emi-seq-inp-bpt-200.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Estrazione pathname 1                   *
      *                      *-----------------------------------------*
           move      p-buf-lin (w-lin)    to   w-buf-wrk-alf          .
           perform   emi-seq-inp-ext-000  thru emi-seq-inp-ext-999    .
           move      w-buf-wrk-ext        to   w-buf-wrk-pt1          .
      *                      *-----------------------------------------*
      *                      * Estrazione pathname 2                   *
      *                      *-----------------------------------------*
           perform   emi-seq-inp-ext-000  thru emi-seq-inp-ext-999    .
           move      w-buf-wrk-ext        to   w-buf-wrk-pt2          .
      *                      *-----------------------------------------*
      *                      * Estrazione pathname 3                   *
      *                      *-----------------------------------------*
           perform   emi-seq-inp-ext-000  thru emi-seq-inp-ext-999    .
           move      w-buf-wrk-ext        to   w-buf-wrk-pt3          .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-buf-num-ele          .
      *                      *-----------------------------------------*
      *                      * Test su contatore                       *
      *                      *-----------------------------------------*
           if        w-buf-num-ele        >    w-buf-max-ele
                     go to emi-seq-inp-bpt-900.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione vera e propria          *
      *                      *-----------------------------------------*
           move      w-buf-wrk-pt1        to   w-buf-eps-pth
                                              (w-buf-num-ele, 01)     .
           move      w-buf-wrk-pt2        to   w-buf-eps-pth
                                              (w-buf-num-ele, 02)     .
           move      w-buf-wrk-pt3        to   w-buf-eps-pth
                                              (w-buf-num-ele, 03)     .
      *                  *---------------------------------------------*
      *                  * Normalizzazione linea trattata              *
      *                  *---------------------------------------------*
           move      spaces               to   p-buf-lin (w-lin)      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     emi-seq-inp-bpt-200.
       emi-seq-inp-bpt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-seq-inp-bpt-999.
       emi-seq-inp-bpt-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo di inizio pagina          *
      *    *                                                           *
      *    * Subroutine di emissione elementi nel buffer               *
      *    *-----------------------------------------------------------*
       emi-seq-inp-ebf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      w-set-top            to   w-set-wtp              .
           move      zero                 to   w-buf-xxx-eps          .
           move      zero                 to   w-buf-yyy-eps          .
           move      spaces               to   w-buf-scl-eps          .
       emi-seq-inp-ebf-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatori                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-ctr-001          .
       emi-seq-inp-ebf-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-buf-ctr-001          .
      *                  *---------------------------------------------*
      *                  * Test se fine buffer                         *
      *                  *---------------------------------------------*
           if        w-buf-ctr-001        >    w-par-eps-wrk-nlf
                     go to emi-seq-inp-ebf-400.
           if        w-buf-ctr-001        >    w-buf-num-ele
                     go to emi-seq-inp-ebf-400.
      *                  *---------------------------------------------*
      *                  * Sub-ciclo da 1 a 'n' (max 3) EPS            *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-ctr-002          .
       emi-seq-inp-ebf-220.
           add       1                    to   w-buf-ctr-002          .
           if        w-buf-ctr-002        >    w-par-eps-wrk-nel
                     go to emi-seq-inp-ebf-280.
           if        w-buf-ctr-002        >    3
                     go to emi-seq-inp-ebf-280.
           if        w-buf-eps-pth
                    (w-buf-ctr-001
                     w-buf-ctr-002)       =    spaces
                     go to emi-seq-inp-ebf-220.
      *                  *---------------------------------------------*
      *                  * Pathname file 'eps'                         *
      *                  *---------------------------------------------*
           move      w-buf-eps-pth
                    (w-buf-ctr-001
                     w-buf-ctr-002)       to   w-pat-eps-nam          .
           perform   emi-seq-inp-pts-000  thru emi-seq-inp-pts-999    .
      *                  *---------------------------------------------*
      *                  * Preparazione parametri di posizionamento    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * 'X'                                     *
      *                      *-----------------------------------------*
           if        w-buf-ctr-002        =    1
                     move  w-par-eps-spc-pe1
                                          to   w-buf-xxx-eps
           else if   w-buf-ctr-002        =    2
                     move  w-par-eps-spc-pe2
                                          to   w-buf-xxx-eps
           else      move  w-par-eps-spc-pe3
                                          to   w-buf-xxx-eps          .
      *                      *-----------------------------------------*
      *                      * 'Y'                                     *
      *                      *-----------------------------------------*
           if        w-buf-ctr-002        >    1
                     go to emi-seq-inp-ebf-240.
           subtract  w-set-fin            from w-set-wtp              .
           move      w-set-wtp            to   w-buf-yyy-eps          .
       emi-seq-inp-ebf-240.
      *                      *-----------------------------------------*
      *                      * Scale                                   *
      *                      *-----------------------------------------*
           if        w-buf-ctr-002        =    1
                     move  w-par-eps-spc-se1
                                          to   w-buf-scl-eps
           else if   w-buf-ctr-002        =    2
                     move  w-par-eps-spc-se2
                                          to   w-buf-scl-eps
           else      move  w-par-eps-spc-se3
                                          to   w-buf-scl-eps          .
      *                  *---------------------------------------------*
      *                  * Inclusione EPS                              *
      *                  *---------------------------------------------*
           perform   emi-seq-inp-eps-000  thru emi-seq-inp-eps-999    .
       emi-seq-inp-ebf-260.
      *                  *---------------------------------------------*
      *                  * Sub-riciclo                                 *
      *                  *---------------------------------------------*
           go to     emi-seq-inp-ebf-220.
       emi-seq-inp-ebf-280.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     emi-seq-inp-ebf-200.
       emi-seq-inp-ebf-400.
       emi-seq-inp-ebf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-seq-inp-ebf-999.
       emi-seq-inp-ebf-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo di inizio pagina          *
      *    *                                                           *
      *    * Estrazione singolo parametro per buffer EPS               *
      *    *-----------------------------------------------------------*
       emi-seq-inp-ext-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-ctr-001          .
           move      spaces               to   w-buf-wrk-al1          .
           move      spaces               to   w-buf-wrk-al2          .
           move      spaces               to   w-buf-wrk-ext          .
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           inspect   w-buf-wrk-alf    tallying w-buf-ctr-001
                                          for  all ";"                .
           if        w-buf-ctr-001        =    zero
                     move  w-buf-wrk-alf  to   w-buf-wrk-ext
                     go to emi-seq-inp-ext-800.
      *              *-------------------------------------------------*
      *              * Estrazione                                      *
      *              *-------------------------------------------------*
           unstring  w-buf-wrk-alf
                                delimited by   ";"
                                          into w-buf-wrk-al1
                                    count in   w-buf-ctr-001          .
           add       2                    to   w-buf-ctr-001          .
      *              *-------------------------------------------------*
      *              * Estrazione stringa rimanente                    *
      *              *-------------------------------------------------*
           unstring  w-buf-wrk-alf        into w-buf-wrk-al2
                                  with pointer w-buf-ctr-001          .
      *              *-------------------------------------------------*
      *              * Valori estratti                                 *
      *              *-------------------------------------------------*
           move      w-buf-wrk-al1        to   w-buf-wrk-ext          .
           move      w-buf-wrk-al2        to   w-buf-wrk-alf          .
       emi-seq-inp-ext-800.
      *              *-------------------------------------------------*
      *              * Eventuali normalizzazioni                       *
      *              *-------------------------------------------------*
           if        w-buf-wrk-ext
                     (01 : 01)            =    "{"
                     move  spaces         to   w-buf-wrk-ext          .
       emi-seq-inp-ext-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-seq-inp-ext-999.
       emi-seq-inp-ext-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo di inizio pagina          *
      *    *                                                           *
      *    * Subroutine per determinazione pathname                    *
      *    *-----------------------------------------------------------*
       emi-seq-inp-pts-000.
      *              *-------------------------------------------------*
      *              * Pathname di base da segreteria                  *
      *              *-------------------------------------------------*
           move      "PB"                 to   s-ope                  .
           move      "asc "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                          using s                     .
           move      s-pat                to   w-pat-eps-pbs          .
      *              *-------------------------------------------------*
      *              * String                                          *
      *              *-------------------------------------------------*
           move      spaces               to   w-pat-eps-pat          .
           string    s-pat      delimited by   spaces
                     "/"        delimited by   size
                     w-pat-eps-nam
                                delimited by   spaces
                     ".eps"     delimited by   size
                                          into w-pat-eps-pat          .
       emi-seq-inp-pts-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-seq-inp-pts-999.
       emi-seq-inp-pts-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo di inizio pagina          *
      *    *                                                           *
      *    * Subroutine per inclusione EPS da buffer                   *
      *    *-----------------------------------------------------------*
       emi-seq-inp-epb-000.
      *              *-------------------------------------------------*
      *              * 'BeginEPSF'                                     *
      *              *-------------------------------------------------*
           move      "%%BeginPageSetup"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/pgsave save def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "%%EndPageSetup"     to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "BeginEPSF"          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/TstXin TstMin neg def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/TstXsi TstMsi neg def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "TstXin TstXsi translate"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-inp-epb-050.
      *              *-------------------------------------------------*
      *              * Posizionamento 'EPS'                            *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-buf-xxx-eps
                                delimited by   size
                     " "        delimited by   size
                     w-buf-yyy-eps
                                delimited by   size
                     " translate"
                                delimited by   size
                                          into stp-rec                .
       emi-seq-inp-epb-058.
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-inp-epb-060.
      *              *-------------------------------------------------*
      *              * Scala 'EPS'                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se in presenza di piu' EPS             *
      *                  *---------------------------------------------*
           if        w-buf-scl-eps        =    spaces
                     move  "EpsScl EpsScl scale"
                                          to   stp-rec
                     go to emi-seq-inp-epb-068.
      *
           move      spaces               to   stp-rec                .
           string    w-buf-scl-eps
                                delimited by   spaces
                     " "        delimited by   size
                     w-buf-scl-eps
                                delimited by   spaces
                     " scale"   delimited by   size
                                          into stp-rec                .
       emi-seq-inp-epb-068.
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-inp-epb-070.
      *              *-------------------------------------------------*
      *              * Altri parametri                                 *
      *              *-------------------------------------------------*
           move      "newpath"            to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "%%BeginDocument: "  to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-inp-epb-100.
      *              *-------------------------------------------------*
      *              * Inizio ciclo di scansione buffer                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-par-eps-wrk-ctb      .
       emi-seq-inp-epb-200.
           add       1                    to   w-par-eps-wrk-ctb      .
           if        w-par-eps-wrk-ctb    >    80
                     go to emi-seq-inp-epb-800.
      *                  *---------------------------------------------*
      *                  * Test su contenuto linea                     *
      *                  *---------------------------------------------*
           if        d-bar-cod-buf-txt
                    (w-par-eps-wrk-ctb)   =    spaces
                     go to emi-seq-inp-epb-800.
      *                  *---------------------------------------------*
      *                  * Scrittura linea                             *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           move      d-bar-cod-buf-txt
                    (w-par-eps-wrk-ctb)   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     emi-seq-inp-epb-200.
       emi-seq-inp-epb-800.
      *              *-------------------------------------------------*
      *              * 'EndEPSF'                                       *
      *              *-------------------------------------------------*
           move      "%%EndDocument"      to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "EndEPSF"            to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "pgsave restore"     to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-inp-epb-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-seq-inp-epb-999.
       emi-seq-inp-epb-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo di inizio pagina          *
      *    *                                                           *
      *    * Subroutine per inclusione EPS                             *
      *    *-----------------------------------------------------------*
       emi-seq-inp-eps-000.
      *              *-------------------------------------------------*
      *              * 'BeginEPSF'                                     *
      *              *-------------------------------------------------*
           move      "%%BeginPageSetup"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/pgsave save def"   to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "%%EndPageSetup"     to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "BeginEPSF"          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/TstXin TstMin neg def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "/TstXsi TstMsi neg def"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "TstXin TstXsi translate"
                                          to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-inp-eps-050.
      *              *-------------------------------------------------*
      *              * Posizionamento 'EPS'                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se in presenza di piu' EPS             *
      *                  *---------------------------------------------*
           if        w-par-eps-spc-flg    not  = "+" or
                     w-buf-xxx-eps        =    zero
                     move  "EpsTsx EpsTsy translate"
                                          to   stp-rec
                     go to emi-seq-inp-eps-058.
      *
           move      spaces               to   stp-rec                .
           string    w-buf-xxx-eps
                                delimited by   size
                     " "        delimited by   size
                     w-buf-yyy-eps
                                delimited by   size
                     " translate"
                                delimited by   size
                                          into stp-rec                .
       emi-seq-inp-eps-058.
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-inp-eps-060.
      *              *-------------------------------------------------*
      *              * Scala 'EPS'                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se in presenza di piu' EPS             *
      *                  *---------------------------------------------*
           if        w-par-eps-spc-flg    not  = "+" or
                     w-buf-scl-eps        =    spaces
                     move  "EpsScl EpsScl scale"
                                          to   stp-rec
                     go to emi-seq-inp-eps-068.
      *
           move      spaces               to   stp-rec                .
           string    w-buf-scl-eps
                                delimited by   spaces
                     " "        delimited by   size
                     w-buf-scl-eps
                                delimited by   spaces
                     " scale"   delimited by   size
                                          into stp-rec                .
       emi-seq-inp-eps-068.
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-inp-eps-070.
      *              *-------------------------------------------------*
      *              * Altri parametri                                 *
      *              *-------------------------------------------------*
           move      "newpath"            to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "%%BeginDocument: "  to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-inp-eps-100.
      *              *-------------------------------------------------*
      *              * Apertura del file in input                      *
      *              *-------------------------------------------------*
           move      "OI"                 to   g-ope                  .
           move      "logo"               to   g-nam                  .
           move      w-pat-eps-pat        to   g-pat                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to emi-seq-inp-eps-200.
       emi-seq-inp-eps-150.
      *              *-------------------------------------------------*
      *              * Generazione del modulo EPS tramite apposito     *
      *              * script                                          *
      *              *                                                 *
      *              * ___ SPERIMENTALE ___                            *
      *              * ___ Problema con pagine successive ___          *
      *              * ___ DISATTIVATO  ___                            *
      *              *-------------------------------------------------*
           go to     emi-seq-inp-eps-800.
      *
           move      spaces               to   o-shs                  .
      *
           string    "t_mod_csv"  
                                delimited by   size
                                          into o-shs                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo 'mopsys'                    *
      *              *-------------------------------------------------*
           move      "SH"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Apertura del file in input                      *
      *              *-------------------------------------------------*
           move      "OI"                 to   g-ope                  .
           move      "logo"               to   g-nam                  .
           move      "/abd/asc/tmp/wipfatX.eps"
                                          to   g-pat                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to emi-seq-inp-eps-200
           else      go to emi-seq-inp-eps-800.
       emi-seq-inp-eps-200.
      *              *-------------------------------------------------*
      *              * Ciclo di lettura file                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura linea                               *
      *                  *---------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Test se at-end                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-end-fil
                     go to emi-seq-inp-eps-800.
      *                  *---------------------------------------------*
      *                  * Scrittura linea appena letta                *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           move      g-rec                to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     emi-seq-inp-eps-200.
       emi-seq-inp-eps-800.
      *              *-------------------------------------------------*
      *              * Chiusura del file in input                      *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * 'EndEPSF'                                       *
      *              *-------------------------------------------------*
           move      "%%EndDocument"      to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "EndEPSF"            to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
      *
           move      "pgsave restore"     to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-inp-eps-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-seq-inp-eps-999.
       emi-seq-inp-eps-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenze di controllo di fine pagina            *
      *    *-----------------------------------------------------------*
       emi-seq-fip-000.
      *              *-------------------------------------------------*
      *              * Stampa dell'array 'Pagina'                      *
      *              *-------------------------------------------------*
           move      "showp restore"      to   stp-rec                .
           perform   wrt-rec-stp-000      thru wrt-rec-stp-999        .
       emi-seq-fip-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione barcode                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/dbarcod0.dts"                   .

