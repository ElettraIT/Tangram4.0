       Identification Division.
       Program-Id.                                 pxpg0002           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/01/01    *
      *                       Ultima revisione:    NdK del 09/06/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Azioni pre-presentazione primo menu' con    *
      *                    riepilogo stampe su disco                   *
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
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det codice azienda in uso                    *
      *        *-------------------------------------------------------*
           05  w-det-cod-azi.
      *            *---------------------------------------------------*
      *            * Codice azienda attualmente in uso                 *
      *            *---------------------------------------------------*
               10  w-det-cod-azi-cod      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det codice utente in uso                     *
      *        *-------------------------------------------------------*
           05  w-det-cod-ute.
      *            *---------------------------------------------------*
      *            * Codice utente attualmente in uso                  *
      *            *---------------------------------------------------*
               10  w-det-cod-ute-cod      pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det esistenza print-files per l'utente in u- *
      *        * so, per l'azienda in uso                              *
      *        *-------------------------------------------------------*
           05  w-det-prf-uaz.
      *            *---------------------------------------------------*
      *            * Numero totale di print-files in catalogo, sia in  *
      *            * fase di creazione che gia' creati                 *
      *            *---------------------------------------------------*
               10  w-det-prf-uaz-tot      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Numero di print-files in fase di creazione        *
      *            *---------------------------------------------------*
               10  w-det-prf-uaz-ifc      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Numero di print-files gia' creati                 *
      *            *---------------------------------------------------*
               10  w-det-prf-uaz-gcr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Numero di print-files gia' creati ma non ancora   *
      *            * effettivamente stampanti                          *
      *            *---------------------------------------------------*
               10  w-det-prf-uaz-cns      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Numero di print-files gia' creati e gia' effetti- *
      *            * vamente stampanti                                 *
      *            *---------------------------------------------------*
               10  w-det-prf-uaz-cgs      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Numero di print-files di tipo 'Messaggio'         *
      *            *---------------------------------------------------*
               10  w-det-prf-uaz-msg      pic  9(05)                  .

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
      *              * Apertura file [pfc]                             *
      *              *-------------------------------------------------*
           perform   opn-fil-pfc-000      thru opn-fil-pfc-999        .
      *              *-------------------------------------------------*
      *              * Det codice azienda attualmente in uso           *
      *              *-------------------------------------------------*
           perform   det-cod-azi-000      thru det-cod-azi-999        .
      *              *-------------------------------------------------*
      *              * Det codice utente attualmente in uso            *
      *              *-------------------------------------------------*
           perform   det-cod-ute-000      thru det-cod-ute-999        .
      *              *-------------------------------------------------*
      *              * Det esistenza print-files in catalogo per l'u-  *
      *              * tente in uso e per l'azienda in uso             *
      *              *-------------------------------------------------*
           perform   det-prf-uaz-000      thru det-prf-uaz-999        .
      *              *-------------------------------------------------*
      *              * Messaggio per print files in catalogo           *
      *              *-------------------------------------------------*
           perform   msg-prf-pfc-000      thru msg-prf-pfc-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Chiusura file [pfc]                             *
      *              *-------------------------------------------------*
           perform   cls-fil-pfc-000      thru cls-fil-pfc-999        .
       main-999.
           exit      program                                          .

      *    *===========================================================*
      *    * Apertura file [pfc]                                       *
      *    *-----------------------------------------------------------*
       opn-fil-pfc-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname per [pfc]                 *
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
           move      "pfc"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-pfc-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di i-o di Open                       *
      *              *-------------------------------------------------*
           open      i-o    pfc                                       .
       opn-fil-pfc-999.
           exit.

      *    *===========================================================*
      *    * Chiusura file [pfc]                                       *
      *    *-----------------------------------------------------------*
       cls-fil-pfc-000.
           close      pfc                                             .
       cls-fil-pfc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione codice azienda in uso                      *
      *    *-----------------------------------------------------------*
       det-cod-azi-000.
      *              *-------------------------------------------------*
      *              * Richiesta informazioni generali da segreteria   *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Estrazione codice azienda                       *
      *              *-------------------------------------------------*
           move      s-azi                to   w-det-cod-azi-cod      .
       det-cod-azi-999.
           exit.

      *    *===========================================================*
      *    * Determinazione codice utente in uso                       *
      *    *-----------------------------------------------------------*
       det-cod-ute-000.
      *              *-------------------------------------------------*
      *              * Richiesta informazioni generali da segreteria   *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Estrazione codice utente                        *
      *              *-------------------------------------------------*
           move      s-ute                to   w-det-cod-ute-cod      .
       det-cod-ute-999.
           exit.

      *    *===========================================================*
      *    * Determinazione esistenza print-files catalogati per l'u-  *
      *    * tente in uso, per l'azienda in uso                        *
      *    *-----------------------------------------------------------*
       det-prf-uaz-000.
      *              *-------------------------------------------------*
      *              * Azzeramento iniziale contatori print-files      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero totale print files                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-prf-uaz-tot      .
      *                  *---------------------------------------------*
      *                  * Numero print files in fase di creazione     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-prf-uaz-ifc      .
      *                  *---------------------------------------------*
      *                  * Numero print files gia' creati              *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-prf-uaz-gcr      .
      *                  *---------------------------------------------*
      *                  * Numero print files gia' creati ma non an-   *
      *                  * cora effettivamente stampati                *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-prf-uaz-cns      .
      *                  *---------------------------------------------*
      *                  * Numero print files gia' creati e gia' ef-   *
      *                  * fettivamente stampati                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-prf-uaz-cgs      .
      *                  *---------------------------------------------*
      *                  * Numero print files di tipo 'Messaggio'      *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-prf-uaz-msg      .
       det-prf-uaz-100.
      *              *-------------------------------------------------*
      *              * Start su [pfc]                                  *
      *              *-------------------------------------------------*
           move      w-det-cod-ute-cod    to   pfc-cod-ute-005        .
           move      w-det-cod-azi-cod    to   pfc-cod-azi-005        .
           move      zero                 to   pfc-num-prg-005        .
           start     pfc    key not less
                            pfc-k05
                            invalid key
                            go to   det-prf-uaz-999.
       det-prf-uaz-200.
      *              *-------------------------------------------------*
      *              * Next su [pfc]                                   *
      *              *-------------------------------------------------*
           read      pfc    next
                            with no lock
                            at end
                            go to   det-prf-uaz-999.
       det-prf-uaz-300.
      *              *-------------------------------------------------*
      *              * Max su [pfc]                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se rottura utente : uscita                  *
      *                  *---------------------------------------------*
           if        pfc-cod-ute          not  = w-det-cod-ute-cod
                     go to det-prf-uaz-999.
      *                  *---------------------------------------------*
      *                  * Se rottura azienda : uscita                 *
      *                  *---------------------------------------------*
           if        pfc-cod-azi          not  = w-det-cod-azi-cod
                     go to det-prf-uaz-999.
       det-prf-uaz-400.
      *              *-------------------------------------------------*
      *              * Sel su [pfc]                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' un Print File : no selezione      *
      *                  *---------------------------------------------*
           if        pfc-tip-fil          not  = 01
                     go to det-prf-uaz-200.
       det-prf-uaz-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento contatori                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero totale print files                   *
      *                  *---------------------------------------------*
           add       1                    to   w-det-prf-uaz-tot      .
      *                  *---------------------------------------------*
      *                  * Numero print files in fase di creazione o   *
      *                  * numero print files gia' creati              *
      *                  *---------------------------------------------*
           if        pfc-dat-fcr          not  = zero
                     add   1              to   w-det-prf-uaz-gcr
           else      add   1              to   w-det-prf-uaz-ifc      .
      *                  *---------------------------------------------*
      *                  * Numero print files gia' creati ma non anco- *
      *                  * ra effettivamente stampati o                *
      *                  * numero print files gia' creati e gia' ef-   *
      *                  * fettivamente stampati                       *
      *                  *---------------------------------------------*
           if        pfc-dat-fcr          not  = zero
                     if    pfc-num-sef    not  = zero
                           add     1      to   w-det-prf-uaz-cgs
                     else  add     1      to   w-det-prf-uaz-cns      .
      *                  *---------------------------------------------*
      *                  * Numero print files di tipo 'Messaggio'      *
      *                  *---------------------------------------------*
           if        pfc-ide-fas          =    "e-mail"
                     add   1              to   w-det-prf-uaz-msg      .
       det-prf-uaz-600.
      *              *-------------------------------------------------*
      *              * Riciclo su next [pfc]                           *
      *              *-------------------------------------------------*
           go to     det-prf-uaz-200.
       det-prf-uaz-999.
           exit.

      *    *===========================================================*
      *    * Messaggio per files in catalogo                           *
      *    *-----------------------------------------------------------*
       msg-prf-pfc-000.
      *              *-------------------------------------------------*
      *              * Se numero totale files in catalogo a zero : u-  *
      *              * scita senza alcun messaggio                     *
      *              *-------------------------------------------------*
           if        w-det-prf-uaz-tot    =    zero
                     go to msg-prf-pfc-999.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 01 : trattini                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 02 : Descrizione azienda centrata         *
      *              *-------------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      s-adx                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 03 : trattini                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 05 : lineette                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 06 : titolo                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    "   Stato attuale degli archivi-stampa su disco rel
      -              "ativi all'utente '"
                                delimited by   size
                     w-det-cod-ute-cod
                                delimited by   spaces
                     "'"        delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 07 : lineette                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 08 : spazi                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 09 : literal e valore                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      74                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero totale di archivi-stampa su disco archiviat
      -              "i .................... :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      09                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      w-det-prf-uaz-tot    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 10 : literal e valore                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      74                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero di archivi-stampa su disco gia' effettivame
      -              "nte stampati.......... :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      10                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      w-det-prf-uaz-cgs    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 11 : literal e valore                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      74                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero di archivi-stampa su disco non ancora stamp
      -              "tivamente stampati ... :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      11                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      w-det-prf-uaz-cns    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 12 : literal e valore                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      74                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero di archivi-stampa su disco attualmente in f
      -              "ase di formazione .... :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      12                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      w-det-prf-uaz-ifc    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 13 : literal e valore                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      74                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero di archivi-stampa di tipo 'Messaggio' .....
      -              "...................... :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      13                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      w-det-prf-uaz-msg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 14 : lineette                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 15 : spazi                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Nota bene se archivi-stampa non ancora effetti- *
      *              * vamente stampati                                *
      *              *-------------------------------------------------*
           if        w-det-prf-uaz-cns    =    zero
                     go to msg-prf-pfc-200.
      *                  *---------------------------------------------*
      *                  * Linea 16                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "N.B. : E' opportuno provvedere alla stampa effetti
      -              "va degli archivi-stampa non   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 17                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       ancora stampati per il recupero dello spazi
      -              "o impegnato.   "    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 19                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       L'operazione di stampa effettiva e' eseguib
      -              "ile mediante il comando '.p'  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 20                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       dal menu'.                       "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       msg-prf-pfc-200.
      *              *-------------------------------------------------*
      *              * Linea 22 : trattini                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 24 : [ ]                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione di un qualsiasi carattere          *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      24                   to   v-lin                  .
           move      78                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       msg-prf-pfc-999.
           exit.
