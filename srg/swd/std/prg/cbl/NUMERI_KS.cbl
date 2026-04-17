      *    *===========================================================*
      *    * Work area per stampa                                      *
      *    *-----------------------------------------------------------*
       01  w-stp.
      *        *-------------------------------------------------------*
      *        * Work area per stampa codice 'king-size'               *
      *        *-------------------------------------------------------*
           05  w-stp-cks.
      *            *---------------------------------------------------*
      *            * Codice in input                                   *
      *            *---------------------------------------------------*
               10  w-stp-cks-cod          pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Linea in input                                    *
      *            *---------------------------------------------------*
               10  w-stp-cks-lin          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Posizione in input                                *
      *            *---------------------------------------------------*
               10  w-stp-cks-pos          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-cks-ctr          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Posizione di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-cks-pss          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Contatore di ridefinizione                        *
      *            *---------------------------------------------------*
               10  w-stp-cks-alf          pic  x(01)                  .
               10  w-stp-cks-num redefines
                   w-stp-cks-alf          pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Carattere in input                                *
      *            *                                                   *
      *            * N.B.: Se a spaces il default e' 'X'               *
      *            *---------------------------------------------------*
               10  w-stp-cks-chr          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work area per stampa numeri e lettere 'king-size'     *
      *        *-------------------------------------------------------*
           05  w-stp-nks.
      *            *---------------------------------------------------*
      *            * Numero in input                                   *
      *            *---------------------------------------------------*
               10  w-stp-nks-num          pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Alfa in input                                     *
      *            *---------------------------------------------------*
               10  w-stp-nks-alf          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Linea in input                                    *
      *            *---------------------------------------------------*
               10  w-stp-nks-lin          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Posizione in input                                *
      *            *---------------------------------------------------*
               10  w-stp-nks-pos          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-nks-ctr          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Linea di comodo                                   *
      *            *---------------------------------------------------*
               10  w-stp-nks-cml          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Stringhe per la stampa                            *
      *            *---------------------------------------------------*
               10  w-stp-nks-str occurs 6 pic  x(06)                  .
      *            *---------------------------------------------------*
      *            * Stringhe per la stampa                            *
      *            *---------------------------------------------------*
               10  w-stp-nks-sal          pic  x(06)                  .
      *            *---------------------------------------------------*
      *            * Carattere in input                                *
      *            *                                                   *
      *            * N.B.: Se a spaces il default e' 'X'               *
      *            *---------------------------------------------------*
               10  w-stp-nks-chr          pic  x(01)                  .





           move      rf-dcp-alf-pro       to   w-stp-cks-cod          .
           move      010                  to   w-stp-cks-lin          .
           move      001                  to   w-stp-cks-pos          .
           move      spaces               to   w-stp-cks-chr          .
           perform   rou-stp-cks-000      thru rou-stp-cks-999        .






      *    *===========================================================*
      *    * Routine di stampa di un codice in formato 'king-size'     *
      *    *                                                           *
      *    * Input  : w-stp-cks-cod = Codice da stampare               *
      *    *                                                           *
      *    *          w-stp-cks-lin = Linea superiore                  *
      *    *                                                           *
      *    *          w-stp-cks-pos = Posizione iniziale               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       rou-stp-cks-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        w-stp-cks-cod        =    spaces
                     go to rou-stp-cks-900.
       rou-stp-cks-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-cks-ctr          .
       rou-stp-cks-200.
           add       1                    to   w-stp-cks-ctr          .
           if        w-stp-cks-ctr        >    14
                     go to rou-stp-cks-900.
           if        w-stp-cks-cod
                    (w-stp-cks-ctr : 1)   =    spaces
                     go to rou-stp-cks-200.
      *              *-------------------------------------------------*
      *              * Routine di stampa di un codice o di un caratte- *
      *              * alfanumerico in formato 'king-size'             *
      *              *-------------------------------------------------*
           move      w-stp-cks-cod
                    (w-stp-cks-ctr : 1)   to   w-stp-cks-alf          .
           if        w-stp-cks-num        numeric
                     move  w-stp-cks-num  to   w-stp-nks-num
                     move  spaces         to   w-stp-nks-alf
           else      move  zero           to   w-stp-nks-num
                     move  w-stp-cks-alf  to   w-stp-nks-alf          .
           move      w-stp-cks-lin        to   w-stp-nks-lin          .
           move      w-stp-cks-pos        to   w-stp-nks-pos          .
           multiply  7                    by   w-stp-cks-ctr
                                        giving w-stp-cks-pss          .
           add       w-stp-cks-pss        to   w-stp-nks-pos          .
           subtract  7                    from w-stp-nks-pos          .
           move      w-stp-cks-chr        to   w-stp-nks-chr          .
           perform   rou-stp-nks-000      thru rou-stp-nks-999        .
       rou-stp-cks-800.
      *              *-------------------------------------------------*
      *              * A riciclo                                       *
      *              *-------------------------------------------------*
           go to     rou-stp-cks-200.
       rou-stp-cks-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-cks-999.
       rou-stp-cks-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa di un numero in formato 'king-size'     *
      *    *                                                           *
      *    * Input  : w-stp-nks-num = Numero da stampare               *
      *    *                                                           *
      *    *          w-stp-nks-alf = Carattere da stampare            *
      *    *                                                           *
      *    *          w-stp-nks-lin = Linea superiore                  *
      *    *                                                           *
      *    *          w-stp-nks-pos = Posizione iniziale               *
      *    *                                                           *
      *    * N.B.: Si presume che il test sulle linee residue sia gia' *
      *    *       stato fatto                                         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       rou-stp-nks-000.
      *              *-------------------------------------------------*
      *              * Test se numero o carattere da stampare          *
      *              *-------------------------------------------------*
           if        w-stp-nks-alf        not  = spaces
                     go to rou-stp-nks-500.
      *              *-------------------------------------------------*
      *              * Formattazione eventuale numero                  *
      *              *-------------------------------------------------*
           if        w-stp-nks-num        =    zero
                     move  10             to   w-stp-nks-num          .
      *              *-------------------------------------------------*
      *              * Test su valore numero da stampare               *
      *              *-------------------------------------------------*
           go to     rou-stp-nks-010
                     rou-stp-nks-020
                     rou-stp-nks-030
                     rou-stp-nks-040
                     rou-stp-nks-050
                     rou-stp-nks-060
                     rou-stp-nks-070
                     rou-stp-nks-080
                     rou-stp-nks-090
                     rou-stp-nks-100  
                     depending            on   w-stp-nks-num          .
       rou-stp-nks-005.
      *              *-------------------------------------------------*
      *              * Numeri                                          *
      *              *-------------------------------------------------*
       rou-stp-nks-010.
      *                  *---------------------------------------------*
      *                  * Numero 1                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      "   X  "             to   w-stp-nks-str (1)      .
           move      "  XX  "             to   w-stp-nks-str (2)      .
           move      " X X  "             to   w-stp-nks-str (3)      .
           move      "   X  "             to   w-stp-nks-str (4)      .
           move      "   X  "             to   w-stp-nks-str (5)      .
           move      "  XXX "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-020.
      *                  *---------------------------------------------*
      *                  * Numero 2                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      "  XX  "             to   w-stp-nks-str (1)      .
           move      " X  X "             to   w-stp-nks-str (2)      .
           move      "    X "             to   w-stp-nks-str (3)      .
           move      "   X  "             to   w-stp-nks-str (4)      .
           move      "  X   "             to   w-stp-nks-str (5)      .
           move      " XXXX "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-030.
      *                  *---------------------------------------------*
      *                  * Numero 3                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      "  XX  "             to   w-stp-nks-str (1)      .
           move      " X  X "             to   w-stp-nks-str (2)      .
           move      "   X  "             to   w-stp-nks-str (3)      .
           move      "    X "             to   w-stp-nks-str (4)      .
           move      " X  X "             to   w-stp-nks-str (5)      .
           move      "  XX  "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-040.
      *                  *---------------------------------------------*
      *                  * Numero 4                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      "   X  "             to   w-stp-nks-str (1)      .
           move      "  XX  "             to   w-stp-nks-str (2)      .
           move      " X X  "             to   w-stp-nks-str (3)      .
           move      "XXXXX "             to   w-stp-nks-str (4)      .
           move      "   X  "             to   w-stp-nks-str (5)      .
           move      "   X  "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-050.
      *                  *---------------------------------------------*
      *                  * Numero 5                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " XXXX "             to   w-stp-nks-str (1)      .
           move      " X    "             to   w-stp-nks-str (2)      .
           move      " XXX  "             to   w-stp-nks-str (3)      .
           move      "    X "             to   w-stp-nks-str (4)      .
           move      " X  X "             to   w-stp-nks-str (5)      .
           move      "  XX  "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-060.
      *                  *---------------------------------------------*
      *                  * Numero 6                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      "  XXX "             to   w-stp-nks-str (1)      .
           move      " X    "             to   w-stp-nks-str (2)      .
           move      " XXX  "             to   w-stp-nks-str (3)      .
           move      " X  X "             to   w-stp-nks-str (4)      .
           move      " X  X "             to   w-stp-nks-str (5)      .
           move      "  XX  "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-070.
      *                  *---------------------------------------------*
      *                  * Numero 7                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " XXXX "             to   w-stp-nks-str (1)      .
           move      "    X "             to   w-stp-nks-str (2)      .
           move      " XXXX "             to   w-stp-nks-str (3)      .
           move      "  X   "             to   w-stp-nks-str (4)      .
           move      " X    "             to   w-stp-nks-str (5)      .
           move      " X    "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-080.
      *                  *---------------------------------------------*
      *                  * Numero 8                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      "  XX  "             to   w-stp-nks-str (1)      .
           move      " X  X "             to   w-stp-nks-str (2)      .
           move      "  XX  "             to   w-stp-nks-str (3)      .
           move      " X  X "             to   w-stp-nks-str (4)      .
           move      " X  X "             to   w-stp-nks-str (5)      .
           move      "  XX  "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-090.
      *                  *---------------------------------------------*
      *                  * Numero 9                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      "  XX  "             to   w-stp-nks-str (1)      .
           move      " X  X "             to   w-stp-nks-str (2)      .
           move      " X  X "             to   w-stp-nks-str (3)      .
           move      "  XXX "             to   w-stp-nks-str (4)      .
           move      "    X "             to   w-stp-nks-str (5)      .
           move      " XXX  "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-100.
      *                  *---------------------------------------------*
      *                  * Numero 0                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      "  XX  "             to   w-stp-nks-str (1)      .
           move      " X  X "             to   w-stp-nks-str (2)      .
           move      " X  X "             to   w-stp-nks-str (3)      .
           move      " X  X "             to   w-stp-nks-str (4)      .
           move      " X  X "             to   w-stp-nks-str (5)      .
           move      "  XX  "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-500.
      *              *-------------------------------------------------*
      *              * Caratteri                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-stp-nks-alf        =    "A"
                     go to rou-stp-nks-50a
           else if   w-stp-nks-alf        =    "B"
                     go to rou-stp-nks-50b
           else if   w-stp-nks-alf        =    "C"
                     go to rou-stp-nks-50c
           else if   w-stp-nks-alf        =    "D"
                     go to rou-stp-nks-50d
           else if   w-stp-nks-alf        =    "E"
                     go to rou-stp-nks-50e
           else if   w-stp-nks-alf        =    "F"
                     go to rou-stp-nks-50f
           else if   w-stp-nks-alf        =    "G"
                     go to rou-stp-nks-50g
           else if   w-stp-nks-alf        =    "H"
                     go to rou-stp-nks-50h
           else if   w-stp-nks-alf        =    "I"
                     go to rou-stp-nks-50i
           else if   w-stp-nks-alf        =    "L"
                     go to rou-stp-nks-50i
           else if   w-stp-nks-alf        =    "M"
                     go to rou-stp-nks-50m
           else if   w-stp-nks-alf        =    "N"
                     go to rou-stp-nks-50n
           else if   w-stp-nks-alf        =    "O"
                     go to rou-stp-nks-50o
           else      go to rou-stp-nks-900.
       rou-stp-nks-50a.
      *                  *---------------------------------------------*
      *                  * Lettera "A"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      "  XX  "             to   w-stp-nks-str (1)      .
           move      " X  X "             to   w-stp-nks-str (2)      .
           move      " XXXX "             to   w-stp-nks-str (3)      .
           move      " X  X "             to   w-stp-nks-str (4)      .
           move      " X  X "             to   w-stp-nks-str (5)      .
           move      " X  X "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50b.
      *                  *---------------------------------------------*
      *                  * Lettera "B"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " XXX  "             to   w-stp-nks-str (1)      .
           move      " X  X "             to   w-stp-nks-str (2)      .
           move      " XXX  "             to   w-stp-nks-str (3)      .
           move      " X  X "             to   w-stp-nks-str (4)      .
           move      " X  X "             to   w-stp-nks-str (5)      .
           move      " XXX  "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50c.
      *                  *---------------------------------------------*
      *                  * Lettera "C"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      "  XX  "             to   w-stp-nks-str (1)      .
           move      " X  X "             to   w-stp-nks-str (2)      .
           move      " X    "             to   w-stp-nks-str (3)      .
           move      " X    "             to   w-stp-nks-str (4)      .
           move      " X  X "             to   w-stp-nks-str (5)      .
           move      "  XX  "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50d.
      *                  *---------------------------------------------*
      *                  * Lettera "D"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " XXX  "             to   w-stp-nks-str (1)      .
           move      " X  X "             to   w-stp-nks-str (2)      .
           move      " X  X "             to   w-stp-nks-str (3)      .
           move      " X  X "             to   w-stp-nks-str (4)      .
           move      " X  X "             to   w-stp-nks-str (5)      .
           move      " XXX  "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50e.
      *                  *---------------------------------------------*
      *                  * Lettera "E"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " XXXX "             to   w-stp-nks-str (1)      .
           move      " X    "             to   w-stp-nks-str (2)      .
           move      " XXX  "             to   w-stp-nks-str (3)      .
           move      " X    "             to   w-stp-nks-str (4)      .
           move      " X    "             to   w-stp-nks-str (5)      .
           move      " XXXX "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50f.
      *                  *---------------------------------------------*
      *                  * Lettera "F"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " XXXX "             to   w-stp-nks-str (1)      .
           move      " X    "             to   w-stp-nks-str (2)      .
           move      " XXX  "             to   w-stp-nks-str (3)      .
           move      " X    "             to   w-stp-nks-str (4)      .
           move      " X    "             to   w-stp-nks-str (5)      .
           move      " X    "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50g.
      *                  *---------------------------------------------*
      *                  * Lettera "G"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      "  XX  "             to   w-stp-nks-str (1)      .
           move      " X  X "             to   w-stp-nks-str (2)      .
           move      " X    "             to   w-stp-nks-str (3)      .
           move      " X XX "             to   w-stp-nks-str (4)      .
           move      " X  X "             to   w-stp-nks-str (5)      .
           move      "  XX  "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50h.
      *                  *---------------------------------------------*
      *                  * Lettera "H"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " X  X "             to   w-stp-nks-str (1)      .
           move      " X  X "             to   w-stp-nks-str (2)      .
           move      " XXXX "             to   w-stp-nks-str (3)      .
           move      " X  X "             to   w-stp-nks-str (4)      .
           move      " X  X "             to   w-stp-nks-str (5)      .
           move      " X  X "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50i.
      *                  *---------------------------------------------*
      *                  * Lettera "I"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " XXX  "             to   w-stp-nks-str (1)      .
           move      "  X   "             to   w-stp-nks-str (2)      .
           move      "  X   "             to   w-stp-nks-str (3)      .
           move      "  X   "             to   w-stp-nks-str (4)      .
           move      "  X   "             to   w-stp-nks-str (5)      .
           move      " XXX  "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50l.
      *                  *---------------------------------------------*
      *                  * Lettera "L"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " X    "             to   w-stp-nks-str (1)      .
           move      " X    "             to   w-stp-nks-str (2)      .
           move      " X    "             to   w-stp-nks-str (3)      .
           move      " X    "             to   w-stp-nks-str (4)      .
           move      " X    "             to   w-stp-nks-str (5)      .
           move      " XXXX "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50m.
      *                  *---------------------------------------------*
      *                  * Lettera "M"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " X   X"             to   w-stp-nks-str (1)      .
           move      " XX XX"             to   w-stp-nks-str (2)      .
           move      " X X X"             to   w-stp-nks-str (3)      .
           move      " X   X"             to   w-stp-nks-str (4)      .
           move      " X   X"             to   w-stp-nks-str (5)      .
           move      " X   X"             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50n.
      *                  *---------------------------------------------*
      *                  * Lettera "N"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " X   X"             to   w-stp-nks-str (1)      .
           move      " XX  X"             to   w-stp-nks-str (2)      .
           move      " X X X"             to   w-stp-nks-str (3)      .
           move      " X  XX"             to   w-stp-nks-str (4)      .
           move      " X   X"             to   w-stp-nks-str (5)      .
           move      " X   X"             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50o.
      *                  *---------------------------------------------*
      *                  * Lettera "O"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      "  XXX "             to   w-stp-nks-str (1)      .
           move      " X   X"             to   w-stp-nks-str (2)      .
           move      " X   X"             to   w-stp-nks-str (3)      .
           move      " X   X"             to   w-stp-nks-str (4)      .
           move      " X   X"             to   w-stp-nks-str (5)      .
           move      "  XXX "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50p.
      *                  *---------------------------------------------*
      *                  * Lettera "P"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " XXX  "             to   w-stp-nks-str (1)      .
           move      " X  X "             to   w-stp-nks-str (2)      .
           move      " XXX  "             to   w-stp-nks-str (3)      .
           move      " X    "             to   w-stp-nks-str (4)      .
           move      " X    "             to   w-stp-nks-str (5)      .
           move      " X    "             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-50q.
      *                  *---------------------------------------------*
      *                  * Lettera "Q"                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione stringhe da stampare       *
      *                      *-----------------------------------------*
           move      " XXX  "             to   w-stp-nks-str (1)      .
           move      "X   X "             to   w-stp-nks-str (2)      .
           move      "X   X "             to   w-stp-nks-str (3)      .
           move      "X  XX "             to   w-stp-nks-str (4)      .
           move      "X   X "             to   w-stp-nks-str (5)      .
           move      " XXX X"             to   w-stp-nks-str (6)      .
      *                      *-----------------------------------------*
      *                      * A sub-ciclo di stampa                   *
      *                      *-----------------------------------------*
           perform   rou-stp-nks-800      thru rou-stp-nks-899        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-nks-900.
       rou-stp-nks-800.
      *              *-------------------------------------------------*
      *              * Sub-ciclo di stampa                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione contatore                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-nks-ctr          .
       rou-stp-nks-820.
           add       1                    to   w-stp-nks-ctr          .
           if        w-stp-nks-ctr        >    6
                     go to rou-stp-nks-899.
      *                  *---------------------------------------------*
      *                  * Posizionamento                              *
      *                  *---------------------------------------------*
           move      w-stp-nks-lin        to   w-stp-nks-cml          .
           add       w-stp-nks-ctr        to   w-stp-nks-cml          . 
           subtract  1                    from w-stp-nks-cml          . 
           move      "VP"                 to   p-ope                  .
           move      w-stp-nks-cml        to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Preparazione tipo carattere                 *
      *                  *---------------------------------------------*
           move      w-stp-nks-str
                    (w-stp-nks-ctr)       to   w-stp-nks-sal          .
           if        w-stp-nks-chr        =    spaces or
                     w-stp-nks-chr        =    "X"
                     go to rou-stp-nks-840.
           inspect   w-stp-nks-sal      replacing all "X"
                                          by   w-stp-nks-chr          .
       rou-stp-nks-840.
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-nks-pos        to   p-pos                  .
           move      w-stp-nks-sal        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riciclo a carattere successivo              *
      *                  *---------------------------------------------*
           go to     rou-stp-nks-820.
       rou-stp-nks-899.
           exit.
       rou-stp-nks-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-nks-999.
       rou-stp-nks-999.
           exit.
