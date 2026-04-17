      *    *===========================================================*
      *    * Subroutines comuni per i moduli CGI (presume wallstr0)    *
      *    *===========================================================*

      *    *===========================================================*
      *    * Operazioni su parametri in input                          *
      *    *-----------------------------------------------------------*
       ope-prm-inp-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-cgi-tip-ope        =    "NO"
                     go to ope-prm-inp-100
           else if   w-cgi-tip-ope        =    "EX"
                     go to ope-prm-inp-500.
       ope-prm-inp-100.
      *              *=================================================*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di normalizzazione                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-cgi-str-ctr          .
       ope-prm-inp-120.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-num
                     go to ope-prm-inp-900.
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to ope-prm-inp-900.
           move      spaces               to   w-cgi-str-fld
                                              (w-cgi-str-ctr)         .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     ope-prm-inp-120.
       ope-prm-inp-500.
      *              *=================================================*
      *              * Estrazione coppie                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di estrazione                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-cgi-str-ctr          .
       ope-prm-inp-520.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-num
                     go to ope-prm-inp-900.
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to ope-prm-inp-900.
      *                  *---------------------------------------------*
      *                  * Estrazione                                  *
      *                  *---------------------------------------------*
           move      w-cgi-str-fld
                    (w-cgi-str-ctr)       to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           perform   ext-prm-ass-000      thru ext-prm-ass-999        .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     ope-prm-inp-520.
       ope-prm-inp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ope-prm-inp-999.
       ope-prm-inp-999.
           exit.

      *    *===========================================================*
      *    * Routine per l'estrazione di max 20 coppie campi/valori da *
      *    * variabile POST letta                                      *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-cgi-str-var     = Variabile POST letta         *
      *    *                                                           *
      *    *          w-cgi-str-del     = Delimitatore (&)             *
      *    *                                                           *
      *    * Output : w-cgi-str-fld (i) = coppie estratte              *
      *    *                                                           *
      *    *          w-cgi-str-num     = Numero coppie estratte       *
      *    *-----------------------------------------------------------*
       cgi-str-ext-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      "&"                  to   w-cgi-str-del          .
           move      zero                 to   w-cgi-str-num          .
           move      zero                 to   w-cgi-str-ctr          .
       cgi-str-ext-200.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to cgi-str-ext-300.
           move      spaces               to   w-cgi-str-fld
                                              (w-cgi-str-ctr)         .
           go to     cgi-str-ext-200.
       cgi-str-ext-300.
      *              *-------------------------------------------------*
      *              * Test preliminare se il campo e' vuoto           *
      *              *-------------------------------------------------*
           if        w-cgi-str-var        =    spaces
                     go to cgi-str-ext-900.
       cgi-str-ext-400.
      *              *-------------------------------------------------*
      *              * Estrazione                                      *
      *              *-------------------------------------------------*
           unstring  w-cgi-str-var
                                delimited by   w-cgi-str-del
                                          into w-cgi-str-fld (01)
                                               w-cgi-str-fld (02)
                                               w-cgi-str-fld (03)
                                               w-cgi-str-fld (04)
                                               w-cgi-str-fld (05)
                                               w-cgi-str-fld (06)
                                               w-cgi-str-fld (07)
                                               w-cgi-str-fld (08)
                                               w-cgi-str-fld (09)
                                               w-cgi-str-fld (10)
                                               w-cgi-str-fld (11)
                                               w-cgi-str-fld (12)
                                               w-cgi-str-fld (13)
                                               w-cgi-str-fld (14)
                                               w-cgi-str-fld (15)
                                               w-cgi-str-fld (16)
                                               w-cgi-str-fld (17)
                                               w-cgi-str-fld (18)
                                               w-cgi-str-fld (19)
                                               w-cgi-str-fld (20)     .
       cgi-str-ext-500.
      *              *-------------------------------------------------*
      *              * Ciclo di verifica numero stringhe estratte      *
      *              *-------------------------------------------------*
           move      zero                 to   w-cgi-str-ctr          .
       cgi-str-ext-600.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to cgi-str-ext-900.
           if        w-cgi-str-fld
                    (w-cgi-str-ctr)       =    spaces
                     go to cgi-str-ext-900.
           add       1                    to   w-cgi-str-num          .
       cgi-str-ext-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     cgi-str-ext-600.
       cgi-str-ext-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cgi-str-ext-999.
       cgi-str-ext-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di eventuale regolarizzazione codice prodotto  *
      *    *                                                           *
      *    * ___ DA ELIMINARE: VEDI 'eleinv00' "DU" ___                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       ext-prm-pro-000.
      *              *-------------------------------------------------*
      *              * Test su valore del campo in input               *
      *              *-------------------------------------------------*
           if        w-cgi-alf-pro        =    spaces
                     go to ext-prm-pro-900.
      *              *-------------------------------------------------*
      *              * Uppercase                                       *
      *              *-------------------------------------------------*
           move      w-cgi-alf-pro        to   w-all-str-alf          .
           move      24                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-cgi-alf-pro          .
       ext-prm-pro-200.
      *              *-------------------------------------------------*
      *              * Test se presente il carattere '%'               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           move      w-cgi-alf-pro        to   w-all-str-alf          .
           move      "%"                  to   w-all-str-del          .
           perform   all-str-chr-000      thru all-str-chr-999        .
      *                  *---------------------------------------------*
      *                  * Se non trovato: ad uscita                   *
      *                  *---------------------------------------------*
           if        w-all-str-num        =    zero
                     go to ext-prm-pro-900.
      *                  *---------------------------------------------*
      *                  * Scomposizione                               *
      *                  *---------------------------------------------*
           move      w-cgi-alf-pro        to   w-all-str-alf          .
           move      "%"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione eventuale 3' componente     *
      *                  *---------------------------------------------*
           move      w-all-str-cat (3)    to    w-cgi-alf-ele         .
      *                  *---------------------------------------------*
      *                  * Test sui componenti                         *
      *                  *---------------------------------------------*
           if        w-all-str-cat (2)
                    (01 : 02)             not  = "2F" and
                     w-all-str-cat (2)
                    (01 : 02)             not  = "2C"
                     go to ext-prm-pro-900.
           move      w-all-str-cat (1)    to   w-exe-prm-vx1          .
           move      w-all-str-cat (2)
                    (03 : 12)             to   w-exe-prm-vx2          .
      *                  *---------------------------------------------*
      *                  * Riassemblaggio                              *
      *                  *---------------------------------------------*
           move      24                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-exe-prm-vx1        to   w-all-str-cat (1)      .
      *
           if        w-all-str-cat (2)
                    (01 : 02)             =   "2F"
                     move      "/"        to   w-all-str-cat (2)
           else      move      ","        to   w-all-str-cat (2)      .
      *
           move      w-exe-prm-vx2        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-cgi-alf-pro          .
      *                  *---------------------------------------------*
      *                  * Test su eventuale terzo componente          *
      *                  *---------------------------------------------*
           if        w-cgi-alf-ele
                    (01 : 02)             not  = "2F" and
                     w-cgi-alf-ele
                    (01 : 02)             not  = "2C"
                     go to ext-prm-pro-900.
      *
           move      w-cgi-alf-pro        to   w-exe-prm-vx1          .
           move      w-cgi-alf-ele
                    (03 : 12)             to   w-exe-prm-vx2          .
      *                  *---------------------------------------------*
      *                  * Riassemblaggio                              *
      *                  *---------------------------------------------*
           move      24                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-exe-prm-vx1        to   w-all-str-cat (1)      .
      *
           if        w-cgi-alf-ele
                    (01 : 02)             =   "2F"
                     move      "/"        to   w-all-str-cat (2)
           else      move      ","        to   w-all-str-cat (2)      .
      *
           move      w-exe-prm-vx2        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-cgi-alf-pro          .
       ext-prm-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-pro-999.
       ext-prm-pro-999.
           exit.

