       Identification Division.
       Program-Id.                                 clivis01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/03/97    *
      *                       Ultima revisione:    NdK del 29/07/02    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   PROVE CGI                                   *
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
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
           
           
      *    *===========================================================*
      *    * File area per [kk0]                                       *
      *    *-----------------------------------------------------------*
       01  comodo.
           05  stringa                    pic  x(120)                 .
           05  conta                      pic  9(07)                  .
           05  contax                     pic  9(07)                  .
           05  cod-cli                    pic  9(07)                  .
           05  parametro1                 pic  x(30)                  .
           05  parametro2                 pic  x(30)                  .
           
           
       01  parametro                      pic  x(60)                  .

      *       Procedure Division             chaining parametro              .
       Procedure Division                           .

       Main Section.
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
           move      zero                 to   contax                 .
           move      spaces               to   parametro1             .
           move      spaces               to   parametro2             .
           unstring  parametro
                                delimited by "&"
                                          into parametro1
                                    count in   contax                 .
           add       2                    to   contax                 .
           unstring  parametro            into parametro2
                                  with pointer contax                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Preparazione start                              *
      *              *-------------------------------------------------*
           move      zero                 to   conta                  .
           if        parametro1  =    "ordinamento=descrizione       "
                     go to main-150
           else      go to main-100.
       main-100.
      *              *-------------------------------------------------*
      *              * Start per codice                                *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCLI    "         to   f-key                  .
           move      zero                 to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : ad uscita             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to main-900.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     main-200.
       main-150.
      *              *-------------------------------------------------*
      *              * Start per codice                                *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RAGKEY    "         to   f-key                  .
           move      spaces               to   rf-cli-rag-key         .
           move      zero                 to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : ad uscita             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to main-900.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     main-200.
           
       main-200.
           add       1                    to   conta                  .
       main-202.
      *              *-------------------------------------------------*
      *              * Next su [cli]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : uscita                        *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       main-210.
           display "<ul>".
           move rf-cli-cod-cli to cod-cli.
           move spaces to stringa.
           string "<li id='" delimited by size
                  cod-cli
                         delimited by size
                  "'><div class='fnd_sel'>"
                         delimited by size
                  rf-cli-rag-soc
                         delimited by size
                  "</div></li>"
                         delimited by size
                                   into stringa                       .
           display   stringa                                          .
           go to main-200.
       main-900.
           display "<a HREF=#inizio><img src=/icons/up.gif></a>"      .
           display "</body>".
       main-950.
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       main-999.
           exit      program.
______*    stop run.

