       Identification Division.
       Program-Id.                                 pdcp401x           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcp                 *
      *                                Settore:    com                 *
      *                                   Fase:    dcp401              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 28/12/94    *
      *                       Ultima revisione:    NdK del 08/06/18    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Interrogazione su archivio prodotti         *
      *                                                                *
      *                    Sottoprogramma per l'espansione di un co-   *
      *                    dice prodotto                               *
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
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [zp1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp1"                          .
      *        *-------------------------------------------------------*
      *        * [zp2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp2"                          .
      *        *-------------------------------------------------------*
      *        * [zp3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp3"                          .
      *        *-------------------------------------------------------*
      *        * [zum]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzum"                          .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                         .
      *        *-------------------------------------------------------*
      *        * [ztv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfztv"                          .
      *        *-------------------------------------------------------*
      *        * [zcs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzcs"                          .
      *        *-------------------------------------------------------*
      *        * [zpv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfzpv"                          .
      *        *-------------------------------------------------------*
      *        * [zps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzps"                          .

      *    *===========================================================*
      *    * Work-area per valori di i.p.c.                            *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Variabili di i.p.c. da stesso livello                 *
      *        *-------------------------------------------------------*
           05  w-ipc-dsl.
      *            *---------------------------------------------------*
      *            * Flag di uscita                                    *
      *            *---------------------------------------------------*
               10  w-ipc-dsl-flg-exi      pic  x(01)                   .
      *            *---------------------------------------------------*
      *            * Codice numerico del prodotto                      *
      *            *---------------------------------------------------*
               10  w-ipc-dsl-num-pro      pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per lettura personalizzazioni                   *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Flag di uscita da lettura personalizzazioni           *
      *        *-------------------------------------------------------*
           05  w-let-prs-flg-exi          pic  x(01)                   .
      *        *-------------------------------------------------------*
      *        * Personalizzazioni per piano dei conti                 *
      *        *-------------------------------------------------------*
           05  w-prs-pdc.
      *            *---------------------------------------------------*
      *            * Numero livelli del piano dei conti                *
      *            *---------------------------------------------------*
               10  w-prs-pdc-num-liv      pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdx], tipo record 01        *
      *        *-------------------------------------------------------*
           05  w-let-arc-x01.
               10  w-let-arc-x01-flg      pic  x(01)                  .
               10  w-let-arc-x01-pro      pic  9(07)                  .
               10  w-let-arc-x01-ctr      pic  9(02)                  .
               10  w-let-arc-x01-des.
                   15  w-let-arc-x01-rig  occurs 10
                                          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdx], tipo record 04        *
      *        *-------------------------------------------------------*
           05  w-let-arc-x04.
               10  w-let-arc-x04-flg      pic  x(01)                  .
               10  w-let-arc-x04-pro      pic  9(07)                  .
               10  w-let-arc-x04-ctr      pic  9(02)                  .
               10  w-let-arc-x04-des.
                   15  w-let-arc-x04-rig  occurs 10
                                          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zp1]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zp1.
               10  w-let-arc-zp1-flg      pic  x(01)                  .
               10  w-let-arc-zp1-cla      pic  9(05)                  .
               10  w-let-arc-zp1-des      pic  x(40)                  .
               10  w-let-arc-zp1-sud      pic  9(02)                  .
               10  w-let-arc-zp1-umi      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zp2]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zp2.
               10  w-let-arc-zp2-flg      pic  x(01)                  .
               10  w-let-arc-zp2-cla      pic  9(05)                  .
               10  w-let-arc-zp2-gru      pic  9(05)                  .
               10  w-let-arc-zp2-des      pic  x(40)                  .
               10  w-let-arc-zp2-sud      pic  9(02)                  .
               10  w-let-arc-zp2-umi      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zp3]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zp3.
               10  w-let-arc-zp3-flg      pic  x(01)                  .
               10  w-let-arc-zp3-cla      pic  9(05)                  .
               10  w-let-arc-zp3-gru      pic  9(05)                  .
               10  w-let-arc-zp3-sgr      pic  9(05)                  .
               10  w-let-arc-zp3-des      pic  x(40)                  .
               10  w-let-arc-zp3-sud      pic  9(02)                  .
               10  w-let-arc-zp3-umi      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zum]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zum.
               10  w-let-arc-zum-flg      pic  x(01)                  .
               10  w-let-arc-zum-cod      pic  x(03)                  .
               10  w-let-arc-zum-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ztv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ztv.
               10  w-let-arc-ztv-flg      pic  x(01)                  .
               10  w-let-arc-ztv-cod      pic  x(03)                  .
               10  w-let-arc-ztv-des      pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zcs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zcs.
               10  w-let-arc-zcs-flg      pic  x(01)                  .
               10  w-let-arc-zcs-tip      pic  9(02)                  .
               10  w-let-arc-zcs-cod      pic  9(05)                  .
               10  w-let-arc-zcs-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zpv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zpv.
               10  w-let-arc-zpv-flg      pic  x(01)                  .
               10  w-let-arc-zpv-tip      pic  9(02)                  .
               10  w-let-arc-zpv-cod      pic  9(05)                  .
               10  w-let-arc-zpv-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zps].01                     *
      *        *-------------------------------------------------------*
           05  w-let-arc-s01.
               10  w-let-arc-s01-flg      pic  x(01)                  .
               10  w-let-arc-s01-cod      pic  9(05)                  .
               10  w-let-arc-s01-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zps].02                     *
      *        *-------------------------------------------------------*
           05  w-let-arc-s02.
               10  w-let-arc-s02-flg      pic  x(01)                  .
               10  w-let-arc-s02-cod      pic  9(05)                  .
               10  w-let-arc-s02-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zps].03                     *
      *        *-------------------------------------------------------*
           05  w-let-arc-s03.
               10  w-let-arc-s03-flg      pic  x(01)                  .
               10  w-let-arc-s03-cod      pic  9(05)                  .
               10  w-let-arc-s03-des      pic  x(20)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zci]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.ltw"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Estrazione variabili di i.p.c. da stesso livel- *
      *              * lo di profondita' applicativa                   *
      *              *-------------------------------------------------*
           perform   ipc-dlp-000          thru ipc-dlp-999            .
      *              *-------------------------------------------------*
      *              * Se anomalie in estrazione variabili di i.p.c. : *
      *              * uscita immediata senza alcuna azione            *
      *              *-------------------------------------------------*
           if        w-ipc-dsl-flg-exi    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
           perform   let-prs-000          thru let-prs-999            .
      *              *-------------------------------------------------*
      *              * Se anomalie in lettura personalizzazioni : u-   *
      *              * scita immediata senza alcuna azione             *
      *              *-------------------------------------------------*
           if        w-let-prs-flg-exi    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   opn-fls-000          thru opn-fls-999            .
      *              *-------------------------------------------------*
      *              * Espansione record                               *
      *              *-------------------------------------------------*
           perform   exp-rec-000          thru exp-rec-999            .
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   cls-fls-000          thru cls-fls-999            .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Lettura i.p.c. da stesso livello di profondita' applica-  *
      *    * tiva                                                      *
      *    *-----------------------------------------------------------*
       ipc-dlp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-ipc-dsl-flg-exi      .
       ipc-dlp-100.
      *              *-------------------------------------------------*
      *              * Estrazione della eventuale variabile di i.p.c.  *
      *              * "num-pro" per codice numerico prodotto          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura variabile, con cancellazione        *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-pro"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se variabile non esistente : ad uscita per  *
      *                  * per errore                                  *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     go to ipc-dlp-900.
      *                  *---------------------------------------------*
      *                  * Se variabile di formato non corretto : ad   *
      *                  * uscita per errore                           *
      *                  *---------------------------------------------*
           if        s-tip                not  = "N"
                     go to ipc-dlp-900.
           if        s-car                not  = 07
                     go to ipc-dlp-900.
           if        s-dec                not  = 00
                     go to ipc-dlp-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valore                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-dsl-num-pro      .
       ipc-dlp-200.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ipc-dlp-999.
       ipc-dlp-900.
      *              *-------------------------------------------------*
      *              * Uscita per errore                               *
      *              *-------------------------------------------------*
           move      "#"                  to   w-ipc-dsl-flg-exi      .
       ipc-dlp-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazioni                                 *
      *    * tiva                                                      *
      *    *-----------------------------------------------------------*
       let-prs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-prs-flg-exi      .
       let-prs-100.
      *              *-------------------------------------------------*
      *              * Numero livelli del piano dei conti              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura personalizzazione                   *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[liv-pdc]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se variabile esistente se ne memorizza il   *
      *                  * valore, altrimenti si pone il valore norma- *
      *                  * lizzato                                     *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-pdc-num-liv
           else      move  3              to   w-prs-pdc-num-liv      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione personalizzazione           *
      *                  *---------------------------------------------*
           if        w-prs-pdc-num-liv    not  = 2
                     move  3              to   w-prs-pdc-num-liv      .
       let-prs-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [pdx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * [zp1]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * [zp2]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * [zp3]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *              *-------------------------------------------------*
      *              * [zum]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zum                 .
      *              *-------------------------------------------------*
      *              * [zci]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *              *-------------------------------------------------*
      *              * [pdc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * [ztv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofztv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ztv                 .
      *              *-------------------------------------------------*
      *              * [zcs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *              *-------------------------------------------------*
      *              * [zpv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *              *-------------------------------------------------*
      *              * [zps]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zps                 .
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [pdx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * [zp1]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * [zp2]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * [zp3]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *              *-------------------------------------------------*
      *              * [zum]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zum                 .
      *              *-------------------------------------------------*
      *              * [zci]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *              *-------------------------------------------------*
      *              * [pdc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * [ztv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofztv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ztv                 .
      *              *-------------------------------------------------*
      *              * [zcs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *              *-------------------------------------------------*
      *              * [zpv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *              *-------------------------------------------------*
      *              * [zps]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zps                 .
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Espansione record                                         *
      *    *-----------------------------------------------------------*
       exp-rec-000.
      *              *-------------------------------------------------*
      *              * Lettura archivio [dcp]                          *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      w-ipc-dsl-num-pro    to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Se record non esistente in [dcp] : uscita imme- *
      *              * diata senza alcuna azione                       *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to exp-rec-999.
       exp-rec-100.
      *              *-------------------------------------------------*
      *              * Lettura records associati a [dcp]               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [pdx] per la descrizione estesa in lingua   *
      *                  * italiana                                    *
      *                  *---------------------------------------------*
           move      rf-dcp-num-pro       to   w-let-arc-x01-pro      .
           perform   let-arc-x01-000      thru let-arc-x01-999        .
      *                  *---------------------------------------------*
      *                  * [pdx] per la descrizione per il listino     *
      *                  *---------------------------------------------*
           move      rf-dcp-num-pro       to   w-let-arc-x04-pro      .
           perform   let-arc-x04-000      thru let-arc-x04-999        .
      *                  *---------------------------------------------*
      *                  * [zp1] per la classe merceologica            *
      *                  *---------------------------------------------*
           move      rf-dcp-cla-pro       to   w-let-arc-zp1-cla      .
           perform   let-arc-zp1-000      thru let-arc-zp1-999        .
      *                  *---------------------------------------------*
      *                  * [zp1] per il gruppo merceologico            *
      *                  *---------------------------------------------*
           move      rf-dcp-cla-pro       to   w-let-arc-zp2-cla      .
           move      rf-dcp-gru-pro       to   w-let-arc-zp2-gru      .
           perform   let-arc-zp2-000      thru let-arc-zp2-999        .
      *                  *---------------------------------------------*
      *                  * [zp3] per il sottogruppo merceologico       *
      *                  *---------------------------------------------*
           move      rf-dcp-cla-pro       to   w-let-arc-zp3-cla      .
           move      rf-dcp-gru-pro       to   w-let-arc-zp3-gru      .
           move      rf-dcp-sgr-pro       to   w-let-arc-zp3-sgr      .
           perform   let-arc-zp3-000      thru let-arc-zp3-999        .
      *                  *---------------------------------------------*
      *                  * [zci] per il codice iva vendite             *
      *                  *---------------------------------------------*
           move      rf-dcp-cod-iva       to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
      *                  *---------------------------------------------*
      *                  * [pdc] per la contropartita vendite          *
      *                  *---------------------------------------------*
           move      rf-dcp-ctp-ven       to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
      *                  *---------------------------------------------*
      *                  * [zum] per l'unita' di misura                *
      *                  *---------------------------------------------*
           move      rf-dcp-umi-ven       to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
      *                  *---------------------------------------------*
      *                  * [ztv] per il tipo di variante per la vendi- *
      *                  *       ta o per la produzione                *
      *                  *---------------------------------------------*
           move      rf-dcp-tip-vve       to   w-let-arc-ztv-cod      .
           perform   let-arc-ztv-000      thru let-arc-ztv-999        .
      *                  *---------------------------------------------*
      *                  * [zcs] per la categoria sconto               *
      *                  *---------------------------------------------*
           move      03                   to   w-let-arc-zcs-tip      .
           move      rf-dcp-cat-scr       to   w-let-arc-zcs-cod      .
           perform   let-arc-zcs-000      thru let-arc-zcs-999        .
      *                  *---------------------------------------------*
      *                  * [zpv] per la categoria provvigione          *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-zpv-tip      .
           move      rf-dcp-cat-pvg       to   w-let-arc-zpv-cod      .
           perform   let-arc-zpv-000      thru let-arc-zpv-999        .
      *                  *---------------------------------------------*
      *                  * [zps] per il codice statistico 1            *
      *                  *---------------------------------------------*
           move      rf-dcp-cod-s01       to   w-let-arc-s01-cod      .
           perform   let-arc-s01-000      thru let-arc-s01-999        .
      *                  *---------------------------------------------*
      *                  * [zps] per il codice statistico 2            *
      *                  *---------------------------------------------*
           move      rf-dcp-cod-s02       to   w-let-arc-s02-cod      .
           perform   let-arc-s02-000      thru let-arc-s02-999        .
      *                  *---------------------------------------------*
      *                  * [zps] per il codice statistico 2            *
      *                  *---------------------------------------------*
           move      rf-dcp-cod-s03       to   w-let-arc-s03-cod      .
           perform   let-arc-s03-000      thru let-arc-s03-999        .
       exp-rec-200.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exp-rec-300.
      *              *-------------------------------------------------*
      *              * Visualizzazioni per espansione                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Box di contenimento                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Codice prodotto  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-dcp-alf-pro       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      rf-dcp-des-pro       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sinonimo prodotto                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Sinonimo         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-dcp-syn-pro       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Unita' di misura                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "Unita' di misura :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      74                   to   v-pos                  .
           move      rf-dcp-umi-ven       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Classe merceologica                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Classe           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-dcp-cla-pro       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-let-arc-zp1-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Gruppo merceologico                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Gruppo           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-dcp-gru-pro       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-let-arc-zp2-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottogruppo merceologico                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Sottogruppo      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      10                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-dcp-sgr-pro       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-let-arc-zp3-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo prodotto                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Tipo prodotto    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      22                   to   v-pos                  .
           if        rf-dcp-tip-pro       =    1
                     move  "Merce                    "
                                          to   v-alf
           else if   rf-dcp-tip-pro       =    2
                     move  "Servizio                 "
                                          to   v-alf
           else if   rf-dcp-tip-pro       =    3
                     move  "Imballo                  "
                                          to   v-alf
           else if   rf-dcp-tip-pro       =    9
                     move  "Extra attivita' aziendale"
                                          to   v-alf
           else      move  all "."        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Codice iva                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Codice iva       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-dcp-cod-iva       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-let-arc-zci-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Contropartita vendite                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Contropartita    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing con appoggio a sinistra     *
      *                          *-------------------------------------*
           move      w-prs-pdc-num-liv    to   w-edt-cod-pdc-liv      .
           move      rf-dcp-ctp-ven       to   w-edt-cod-pdc-cod      .
           move      "<B"                 to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-let-arc-pdc-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prezzo di listino base                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Prezzo di listino:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      rf-dcp-dec-vlt       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      16                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-dcp-prz-lst       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Variante per la vendita o per la produzione *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      44                   to   v-pos                  .
           move      "Variante :"         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      rf-dcp-tip-vve       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      63                   to   v-pos                  .
           move      w-let-arc-ztv-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Categoria di sconto                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Categ. di sconto :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-dcp-cat-scr       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-let-arc-zcs-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Max 5 % di sconto                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      51                   to   v-pos                  .
           move      "% :"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore % di sconto 1                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      rf-dcp-per-scr (1)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore % di sconto 2                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      60                   to   v-pos                  .
           move      rf-dcp-per-scr (2)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore % di sconto 3                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      rf-dcp-per-scr (3)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore % di sconto 4                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      rf-dcp-per-scr (4)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore % di sconto 5                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      rf-dcp-per-scr (5)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Categoria di provvigione                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Categ. provvig.  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-dcp-cat-pvg       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-let-arc-zpv-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Max 1 % di provvigione                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      51                   to   v-pos                  .
           move      "% :"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore % di provvigione 1               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      rf-dcp-per-pvg (1)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dati inserimento/ultima modifica            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Immissione       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valori                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Data                                *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      20                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-dcp-ide-dat       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Utente                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "["        delimited by   size
                     rf-dcp-ide-ute
                                delimited by   spaces
                     "]"        delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Programma                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "["        delimited by   size
                     rf-dcp-ide-fas
                                delimited by   spaces
                     "]"        delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exp-rec-400.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exp-rec-500.
      *              *-------------------------------------------------*
      *              * Interazioni con l'utente                        *
      *              *-------------------------------------------------*





______*


       exp-rec-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exp-rec-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [pdx], tipo record 01             *
      *    *-----------------------------------------------------------*
       let-arc-x01-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-x01-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-let-arc-x01-ctr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-x01-des      .
       let-arc-x01-100.
      *              *-------------------------------------------------*
      *              * Start su file [pdx]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "TRCLNG    "         to   f-key                  .
           move      01                   to   rf-pdx-tip-rec         .
           move      zero                 to   rf-pdx-cod-arc         .
           move      "I  "                to   rf-pdx-cod-lng         .
           move      w-let-arc-x01-pro    to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      zero                 to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-x01-800.
       let-arc-x01-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [pdx]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-x01-800.
       let-arc-x01-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-pdx-tip-rec       not  = 01                or
                     rf-pdx-cod-arc       not  = zero              or
                     rf-pdx-cod-lng       not  = "I  "             or
                     rf-pdx-cod-num       not  = w-let-arc-x01-pro or
                     rf-pdx-for-mat       not  = spaces
                     go to let-arc-x01-800.
       let-arc-x01-400.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-let-arc-x01-ctr      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdx-des-pro       to   w-let-arc-x01-rig
                                              (w-let-arc-x01-ctr)     .
       let-arc-x01-600.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura                              *
      *              *-------------------------------------------------*
           go to     let-arc-x01-200.
       let-arc-x01-800.
      *              *-------------------------------------------------*
      *              * Test su contatore elementi letti                *
      *              *-------------------------------------------------*
           if        w-let-arc-x01-ctr    =    zero
                     go to let-arc-x01-820
           else      go to let-arc-x01-900.
       let-arc-x01-820.
      *                  *---------------------------------------------*
      *                  * Se nessun elemento rilevato                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di nessun elemento rilevato        *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-x01-flg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     let-arc-x01-900.
       let-arc-x01-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-x01-999.
       let-arc-x01-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [pdx], tipo record 04             *
      *    *-----------------------------------------------------------*
       let-arc-x04-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-x04-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-let-arc-x04-ctr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-x04-des      .
       let-arc-x04-100.
      *              *-------------------------------------------------*
      *              * Start su file [pdx]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "TRCLNG    "         to   f-key                  .
           move      04                   to   rf-pdx-tip-rec         .
           move      zero                 to   rf-pdx-cod-arc         .
           move      "I  "                to   rf-pdx-cod-lng         .
           move      w-let-arc-x04-pro    to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      zero                 to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-x04-800.
       let-arc-x04-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [pdx]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-x04-800.
       let-arc-x04-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-pdx-tip-rec       not  = 04                or
                     rf-pdx-cod-arc       not  = zero              or
                     rf-pdx-cod-lng       not  = "I  "             or
                     rf-pdx-cod-num       not  = w-let-arc-x04-pro or
                     rf-pdx-for-mat       not  = spaces
                     go to let-arc-x04-800.
       let-arc-x04-400.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-let-arc-x04-ctr      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdx-des-pro       to   w-let-arc-x04-rig
                                              (w-let-arc-x04-ctr)     .
       let-arc-x04-600.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura                              *
      *              *-------------------------------------------------*
           go to     let-arc-x04-200.
       let-arc-x04-800.
      *              *-------------------------------------------------*
      *              * Test su contatore elementi letti                *
      *              *-------------------------------------------------*
           if        w-let-arc-x04-ctr    =    zero
                     go to let-arc-x04-820
           else      go to let-arc-x04-900.
       let-arc-x04-820.
      *                  *---------------------------------------------*
      *                  * Se nessun elemento rilevato                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di nessun elemento rilevato        *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-x04-flg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     let-arc-x04-900.
       let-arc-x04-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-x04-999.
       let-arc-x04-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zp1]                             *
      *    *-----------------------------------------------------------*
       let-arc-zp1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp1-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice classe a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zp1-cla    =    zero
                     go to let-arc-zp1-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-arc-zp1-cla    to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zp1-400.
       let-arc-zp1-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zp1-des-cla       to   w-let-arc-zp1-des      .
           move      rf-zp1-ult-sud       to   w-let-arc-zp1-sud      .
           move      rf-zp1-umi-def       to   w-let-arc-zp1-umi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zp1-999.
       let-arc-zp1-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zp1-flg      .
           move      all   "."            to   w-let-arc-zp1-des      .
           go to     let-arc-zp1-600.
       let-arc-zp1-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp1-des      .
       let-arc-zp1-600.
           move      zero                 to   w-let-arc-zp1-sud      .
           move      spaces               to   w-let-arc-zp1-umi      .
       let-arc-zp1-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zp2]                             *
      *    *-----------------------------------------------------------*
       let-arc-zp2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp2-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice gruppo a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zp2-gru    =    zero
                     go to let-arc-zp2-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-let-arc-zp2-cla    to   rf-zp2-cod-cla         .
           move      w-let-arc-zp2-gru    to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zp2-400.
       let-arc-zp2-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zp2-des-gru       to   w-let-arc-zp2-des      .
           move      rf-zp2-ult-sud       to   w-let-arc-zp2-sud      .
           move      rf-zp2-umi-def       to   w-let-arc-zp2-umi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zp2-999.
       let-arc-zp2-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zp2-flg      .
           move      all   "."            to   w-let-arc-zp2-des      .
           go to     let-arc-zp2-600.
       let-arc-zp2-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp2-des      .
       let-arc-zp2-600.
           move      zero                 to   w-let-arc-zp2-sud      .
           move      spaces               to   w-let-arc-zp2-umi      .
       let-arc-zp2-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zp3]                             *
      *    *-----------------------------------------------------------*
       let-arc-zp3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp3-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottogruppo a zero               *
      *              *-------------------------------------------------*
           if        w-let-arc-zp3-sgr    =    zero
                     go to let-arc-zp3-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      w-let-arc-zp3-cla    to   rf-zp3-cod-cla         .
           move      w-let-arc-zp3-gru    to   rf-zp3-cod-gru         .
           move      w-let-arc-zp3-sgr    to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zp3-400.
       let-arc-zp3-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zp3-des-sgr       to   w-let-arc-zp3-des      .
           move      rf-zp3-ult-sud       to   w-let-arc-zp3-sud      .
           move      rf-zp3-umi-def       to   w-let-arc-zp3-umi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zp3-999.
       let-arc-zp3-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zp3-flg      .
           move      all   "."            to   w-let-arc-zp3-des      .
           go to     let-arc-zp3-600.
       let-arc-zp3-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp3-des      .
       let-arc-zp3-600.
           move      zero                 to   w-let-arc-zp3-sud      .
           move      spaces               to   w-let-arc-zp3-umi      .
       let-arc-zp3-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zum]                             *
      *    *-----------------------------------------------------------*
       let-arc-zum-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zum-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice unita' di misura a spaces        *
      *              *-------------------------------------------------*
           if        w-let-arc-zum-cod    =    spaces
                     go to let-arc-zum-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODUMI    "         to   f-key                  .
           move      w-let-arc-zum-cod    to   rf-zum-cod-umi         .
           move      "pgm/dcp/fls/ioc/obj/iofzum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zum                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zum-400.
       let-arc-zum-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zum-des-umi       to   w-let-arc-zum-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zum-999.
       let-arc-zum-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zum-flg      .
           move      all   "."            to   w-let-arc-zum-des      .
           go to     let-arc-zum-999.
       let-arc-zum-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zum-des      .
       let-arc-zum-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zci]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [pdc]                         *
      *    *-----------------------------------------------------------*
       let-arc-pdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottoconto a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-pdc-cod    =    zero
                     go to let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPDC"             to   f-key                  .
           move      w-let-arc-pdc-cod    to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-pdc-400.
       let-arc-pdc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdc-des-pdc       to   w-let-arc-pdc-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-pdc-999.
       let-arc-pdc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-pdc-flg      .
           move      all   "."            to   w-let-arc-pdc-des      .
           go to     let-arc-pdc-999.
       let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-des      .
       let-arc-pdc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [ztv]                         *
      *    *-----------------------------------------------------------*
       let-arc-ztv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ztv-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice variante a spaces                *
      *              *-------------------------------------------------*
           if        w-let-arc-ztv-cod    =    spaces
                     go to let-arc-ztv-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TIPVAR    "         to   f-key                  .
           move      w-let-arc-ztv-cod    to   rf-ztv-tip-var         .
           move      "pgm/dcp/fls/ioc/obj/iofztv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ztv                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-ztv-400.
       let-arc-ztv-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ztv-des-var       to   w-let-arc-ztv-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ztv-999.
       let-arc-ztv-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ztv-flg      .
           move      all   "."            to   w-let-arc-ztv-des      .
           go to     let-arc-ztv-999.
       let-arc-ztv-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ztv-des      .
       let-arc-ztv-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zcs]                         *
      *    *-----------------------------------------------------------*
       let-arc-zcs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcs-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sconto a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zcs-cod    =    zero
                     go to let-arc-zcs-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCSC    "         to   f-key                  .
           move      w-let-arc-zcs-tip    to   rf-zcs-tip-csc         .
           move      w-let-arc-zcs-cod    to   rf-zcs-cod-csc         .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zcs-400.
       let-arc-zcs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zcs-des-csc       to   w-let-arc-zcs-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zcs-999.
       let-arc-zcs-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zcs-flg      .
           move      all   "."            to   w-let-arc-zcs-des      .
           go to     let-arc-zcs-999.
       let-arc-zcs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcs-des      .
       let-arc-zcs-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zpv]                         *
      *    *-----------------------------------------------------------*
       let-arc-zpv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zpv-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sconto a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zpv-cod    =    zero
                     go to let-arc-zpv-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV    "         to   f-key                  .
           move      w-let-arc-zpv-tip    to   rf-zpv-tip-cpv         .
           move      w-let-arc-zpv-cod    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zpv-400.
       let-arc-zpv-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zpv-des-cpv       to   w-let-arc-zpv-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zpv-999.
       let-arc-zpv-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zpv-flg      .
           move      all   "."            to   w-let-arc-zpv-des      .
           go to     let-arc-zpv-999.
       let-arc-zpv-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zpv-des      .
       let-arc-zpv-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zps].01                      *
      *    *-----------------------------------------------------------*
       let-arc-s01-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-s01-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice statistico a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-s01-cod    =    zero
                     go to let-arc-s01-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLS    "         to   f-key                  .
           move      01                   to   rf-zps-tip-cls         .
           move      w-let-arc-s01-cod    to   rf-zps-cod-cls         .
           move      "pgm/dcp/fls/ioc/obj/iofzps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zps                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-s01-400.
       let-arc-s01-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zps-des-cls       to   w-let-arc-s01-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-s01-999.
       let-arc-s01-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-s01-flg      .
           move      all   "."            to   w-let-arc-s01-des      .
           go to     let-arc-s01-999.
       let-arc-s01-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-s01-des      .
       let-arc-s01-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zps].02                      *
      *    *-----------------------------------------------------------*
       let-arc-s02-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-s02-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice statistico a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-s02-cod    =    zero
                     go to let-arc-s02-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLS    "         to   f-key                  .
           move      02                   to   rf-zps-tip-cls         .
           move      w-let-arc-s02-cod    to   rf-zps-cod-cls         .
           move      "pgm/dcp/fls/ioc/obj/iofzps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zps                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-s02-400.
       let-arc-s02-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zps-des-cls       to   w-let-arc-s02-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-s02-999.
       let-arc-s02-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-s02-flg      .
           move      all   "."            to   w-let-arc-s02-des      .
           go to     let-arc-s02-999.
       let-arc-s02-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-s02-des      .
       let-arc-s02-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zps].03                      *
      *    *-----------------------------------------------------------*
       let-arc-s03-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-s03-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice statistico a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-s03-cod    =    zero
                     go to let-arc-s03-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLS    "         to   f-key                  .
           move      03                   to   rf-zps-tip-cls         .
           move      w-let-arc-s03-cod    to   rf-zps-cod-cls         .
           move      "pgm/dcp/fls/ioc/obj/iofzps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zps                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-s03-400.
       let-arc-s03-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zps-des-cls       to   w-let-arc-s03-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-s03-999.
       let-arc-s03-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-s03-flg      .
           move      all   "."            to   w-let-arc-s03-des      .
           go to     let-arc-s03-999.
       let-arc-s03-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-s03-des      .
       let-arc-s03-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .
