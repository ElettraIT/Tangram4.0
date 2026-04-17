       Identification Division.
       Program-Id.                                 loadvar            .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    loadvar             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/01/00    *
      *                       Ultima revisione:    NdK del 10/04/00    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:    Prove di accesso tramite Flash 5.0         *
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

       Special-Names.       Decimal-Point is comma .
       
      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [auc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  auc   assign to disk         f-auc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is auc-key
                             file status  is f-auc-sts                .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [auc]                                    *
      *    *-----------------------------------------------------------*
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
       Working-Storage Section.
      *================================================================*
      
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
      *    * Work per records di [auc] 'ute'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucute0.cpw"                   .

      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * Parametri in input                                    *
      *        *-------------------------------------------------------*
       01  comodo.
           05  contax                     pic  9(07)                  .
           05  user                       pic  x(08)                  .
           05  pasw                       pic  x(08)                  .
           05  parametro1                 pic  x(200)                 .
           05  parametro1b                pic  x(80)                  .
           05  parametro1c                pic  x(80)                  .
           05  parametro2                 pic  x(200)                 .
           05  parametro2b                pic  x(80)                  .
           05  parametro2c                pic  x(80)                  .
           05  uscita                     pic  x(200)                 .
      *        *-------------------------------------------------------*
      *        * Stato di uscita                                       *
      *        *  - "em" : Utente e password mancanti                  *
      *        *  - "uu" : Utente non codificato                       *
      *        *  - "pr" : Password necessaria                         *
      *        *  - "wp" : Password errata                             *
      *        *  - "ok" : Accesso consentito                          *
      *        *  - "ko" : Accesso negato                              *
      *        *-------------------------------------------------------*
           05  exit-sts                   pic  x(02)                  .

      *    *===========================================================*
      *    * Area per parametri di 'chaining' dal chiamante            *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * Variabile di environment V_ALTER_HOME                 *
      *        *-------------------------------------------------------*
       77  parametro                      pic  x(200)                 .

      ******************************************************************
       Procedure Division             chaining parametro              .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   contax                 .
           move      spaces               to   parametro1             .
           move      spaces               to   parametro1b            .
           move      spaces               to   parametro1c            .
           move      spaces               to   parametro2             .
           move      spaces               to   parametro2b            .
           move      spaces               to   parametro2c            .
      *              *-------------------------------------------------*
      *              * Apertura file [auc]                             *
      *              *-------------------------------------------------*
           perform   opn-fil-auc-000      thru opn-fil-auc-999        .
      *              *-------------------------------------------------*
      *              * Estrazione 2 parametri                          *
      *              *-------------------------------------------------*
           unstring  parametro
                                delimited by "&"
                                          into parametro1
                                    count in   contax                 .
           add       2                    to   contax                 .
           unstring  parametro            into parametro2
                                  with pointer contax                 .

           move      zero                 to   contax                 .
           unstring  parametro2
                                delimited by "&"
                                          into parametro2
                                    count in   contax                 .
      *              *-------------------------------------------------*
      *              * Estrazione user                                 *
      *              *-------------------------------------------------*
           unstring  parametro1 delimited by "="
                                          into parametro1b
                                    count in   contax                 .
           add       2                    to   contax                 .
           unstring  parametro1           into parametro1c
                                  with pointer contax                 .
           move      parametro1c          to   user                   .
      *              *-------------------------------------------------*
      *              * Estrazione pwd                                  *
      *              *-------------------------------------------------*
           unstring  parametro2 delimited by "="
                                          into parametro2b
                                    count in   contax                 .
           add       2                    to   contax                 .
           unstring  parametro2           into parametro2c
                                  with pointer contax                 .
           move      parametro2c          to   pasw                   .
      *              *-------------------------------------------------*
      *              * Se il chiamante ha passato 'usr' e 'psw' vuoti  *
      *              *-------------------------------------------------*
           if        user                 =    spaces and
                     pasw                 =    spaces
                     move  "em"           to   exit-sts
                     go to main-200.
      *              *-------------------------------------------------*
      *              * Lettura codice utente                           *
      *              *-------------------------------------------------*
           move      spaces               to   exit-sts               .
           move      user                 to   w-ute-cod-ute          .
           perform   let-ute-auc-000      thru let-ute-auc-999        .
      *              *-------------------------------------------------*
      *              * Se il chiamante non ha passato 'usr' e 'psw',   *
      *              * lettura andata male                             *
      *              *-------------------------------------------------*
           if        parametro1b          not  = "usr" or
                     parametro2b          not  = "psw"
                     move  "ko"           to   exit-sts               .
       main-200.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              * nb.: massimo :co#256: caratteri per output      *
      *              * vedi a_termcap                                  *
      *              *-------------------------------------------------*
           display   "Content-type: text/html" with no advancing      .
           display   ""                                               .
           move      spaces               to   uscita                 .
           string    "snx="     delimited by size
                     exit-sts   delimited by size
                     "&azi=prv&"
                                delimited by size
                                         into uscita                  .
           display   uscita                                           .
      *                  *---------------------------------------------*
      *                  * Chiusura file [auc]                         *
      *                  *---------------------------------------------*
           perform   cls-fil-auc-000      thru cls-fil-auc-999        .
       main-999.
           exit      program.
           stop run.

      *    *===========================================================*
      *    * Open file [auc]                                           *
      *    *-----------------------------------------------------------*
       opn-fil-auc-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname per [auc]                 *
      *              *-------------------------------------------------*
           move      "/abd/fpx/auc"       to   f-auc-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di i-o di Open                       *
      *              *-------------------------------------------------*
           open      i-o    auc                                       .
       opn-fil-auc-999.
           exit.

      *    *===========================================================*
      *    * Close file [auc]                                          *
      *    *-----------------------------------------------------------*
       cls-fil-auc-000.
           close      auc                                             .
       cls-fil-auc-999.
           exit.

      *    *===========================================================*
      *    * Lettura parametri correlati all'utente in w-ute-cod-ute   *
      *    *-----------------------------------------------------------*
       let-ute-auc-000.
      *              *-------------------------------------------------*
      *              * Lettura record codice utente                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "ute "               to   auc-tre                .
           move      w-ute-cod-ute        to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            go to   let-ute-auc-100.
           go to     let-ute-auc-200.
       let-ute-auc-100.
      *                  *---------------------------------------------*
      *                  * Se codice utente non esistente              *
      *                  *---------------------------------------------*
           move      "uu"                 to   exit-sts               .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     let-ute-auc-999.
       let-ute-auc-200.
      *                  *---------------------------------------------*
      *                  * Se codice utente esistente                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Controllo password                      *
      *                      *-----------------------------------------*
           move      auc-dat              to   w-ute                  .
           if        w-ute-pwd-ute        =    pasw
                     move  "ok"           to   exit-sts
           else if   pasw                 =    spaces and
                     w-ute-pwd-ute        not  = spaces
                     move  "pr"           to   exit-sts
           else      move  "wp"           to   exit-sts               .
       let-ute-auc-999.
           exit.


