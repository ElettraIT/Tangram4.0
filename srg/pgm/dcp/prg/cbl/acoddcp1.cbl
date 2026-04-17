       Identification Division.
       Program-Id.                                 acoddcp1           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcp                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 26/05/93    *
      *                       Ultima revisione:    NdK del 30/09/01    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo integrativo di acoddcp0 per richia-  *
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
      *    * Link-area per accettazione codice prodotto                *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-cod-dcp
                                               v
                                               s                      .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      w-cod-cod-dcp-alf    to   w-cod-cod-dcp-s02      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della overlay da esegui- *
      *              * re                                              *
      *              *-------------------------------------------------*
           if        w-cod-cod-dcp-of2    =    "O"
                     go to main-100
           else      go to main-200.
       main-100.
      *              *-------------------------------------------------*
      *              * Se diponibilita' da ordini clienti              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Put della variabile di i.p.c.: 'cod-dpz'    *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-dpz"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      01                   to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Put della variabile di i.p.c.: 'cod-pro'    *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-pro"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-cod-cod-dcp-num    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Put della variabile di i.p.c.: 'dat-ela'    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione data attuale             *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Esecuzione                              *
      *                      *-----------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "dat-ela"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "D"                  to   s-tip                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Richiamo del programma di interrogazione    *
      *                  *---------------------------------------------*
           move      "pgm/orc/prg/obj/porc500s"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     main-999.
       main-200.
      *              *-------------------------------------------------*
      *              * Se diponibilita' da magazzino                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura della variabile di i.p.c. per am- *
      *                  * missibilita' del tasto Slct                 *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "N"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Scrittura della variabile di i.p.c. per ti- *
      *                  * po di interrogazione                        *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      10                   to   s-car                  .
           move      "DSPPDV    "         to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Scrittura della variabile di i.p.c. per co- *
      *                  * dice prodotto                               *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-pro"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-cod-cod-dcp-num    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Scrittura della variabile di i.p.c. per la  *
      *                  * data disponibilita'                         *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "dat-dsp"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "D"                  to   s-tip                  .
           move      zero                 to   s-dat                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Richiamo del programma di interrogazione    *
      *                  *---------------------------------------------*
           move      "pgm/mag/prg/obj/pmag3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     main-999.
       main-999.
           exit      program                                          .

