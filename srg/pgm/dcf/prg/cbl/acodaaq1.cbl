       Identification Division.
       Program-Id.                                 acodaaq1           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    aaq                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 28/05/93    *
      *                       Ultima revisione:    NdK del 13/09/05    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo integrativo di acodaaq0 per richia-  *
      *                    mo programma di interrogazione disponibili- *
      *                    ta' di magazzino                            *
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
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto d'acquisto     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acodaaq0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-cod-aaq
                                               v
                                               s                      .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Scrittura della variabile di i.p.c. per ammis-  *
      *              * sibilita' del tasto Slct                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "N"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Scrittura della variabile di i.p.c. per tipo di *
      *              * interrogazione                                  *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      10                   to   s-car                  .
           if        w-cod-cod-aaq-tco    =    01
                     move  "DSPPDV    "   to   s-alf
           else if   w-cod-cod-aaq-tco    =    03
                     move  "DSPMAP    "   to   s-alf
           else      move  spaces         to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Scrittura della variabile di i.p.c. per codice  *
      *              * prodotto                                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           if        w-cod-cod-aaq-tco    =    01
                     move  "cod-pro"      to   s-var
           else if   w-cod-cod-aaq-tco    =    03
                     move  "cod-map"      to   s-var
           else      move  spaces         to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-cod-cod-aaq-num    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Scrittura della variabile di i.p.c. per *
      *                      * data disponibilita'                     *
      *                      *-----------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "dat-dsp"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "D"                  to   s-tip                  .
           move      zero                 to   s-dat                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Richiamo del programma di interrogazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           move      "pgm/mag/prg/obj/pmag3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       main-999.
           exit      program                                          .

