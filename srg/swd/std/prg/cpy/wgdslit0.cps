      *    *===========================================================*
      *    * Routine di determinazione del giorno della settimana      *
      *    *-----------------------------------------------------------*
      *    * In input  : w-det-dow-dat = Data completa                 *
      *    *                                                           *
      *    * In output : w-det-dow-lit = Giorno della settimana, lite- *
      *    *                             ral                           *
      *    *                                                           *
      *    *             w-det-dow-num = Giorno della settimana, nu-   *
      *    *                             mero secondo la tabella :     *
      *    *                                                           *
      *    *                             -  1 : Lunedi'                *
      *    *                             -  2 : Martedi'               *
      *    *                             -  3 : Mercoledi'             *
      *    *                             -  4 : Giovedi'               *
      *    *                             -  5 : Venerdi'               *
      *    *                             -  6 : Sabato                 *
      *    *                             -  7 : Domenica               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-dow-lit-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-dow-lit          .
           move      zero                 to   w-det-dow-num          .
           move      zero                 to   w-det-dow-ann          .
           move      spaces               to   w-det-dow-snb          .
           move      zero                 to   w-det-dow-c01          .
           move      zero                 to   w-det-dow-c02          .
           move      zero                 to   w-det-dow-c03          .
           move      zero                 to   w-det-dow-c04          .
           move      zero                 to   w-det-dow-c05          .
           move      zero                 to   w-det-dow-c06          .
      *              *-------------------------------------------------*
      *              * Test iniziale che la data in input non sia a    *
      *              * zero                                            *
      *              *-------------------------------------------------*
           if        w-det-dow-dat        =    zero
                     go to det-dow-lit-999.
       det-dow-lit-010.
      *              *-------------------------------------------------*
      *              * Preparazioni iniziali                           *
      *              *-------------------------------------------------*
           move      w-det-dow-saa        to   w-det-dow-aes          .
           add       1900                 to   w-det-dow-aes          .
      *                  *---------------------------------------------*
      *                  * Determinazione se anno bisestile            *
      *                  *---------------------------------------------*
           move      w-det-dow-ann        to   w-det-dow-x03          .
           if        w-det-dow-x03        <    40
                     go to det-dow-lit-040.
       det-dow-lit-030.
           subtract  40                   from w-det-dow-x03          .
           if        w-det-dow-x03        not  < 40
                     go to det-dow-lit-030.
       det-dow-lit-040.
           if        w-det-dow-x03        <    4
                     go to det-dow-lit-060.
       det-dow-lit-050.
           subtract  4                    from w-det-dow-x03          .
           if        w-det-dow-x03        not  < 4
                     go to det-dow-lit-050.
       det-dow-lit-060.
           if        w-det-dow-x03        =    zero
                     move  "#"            to   w-det-dow-snb          .
       det-dow-lit-100.
      *              *-------------------------------------------------*
      *              * Preparazione variabili                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione relativa al mese             *
      *                  *---------------------------------------------*
           if        w-det-dow-mes        =    01
                     move  01             to   w-det-dow-c01
           else if   w-det-dow-mes        =    02
                     move  04             to   w-det-dow-c01
           else if   w-det-dow-mes        =    03
                     move  04             to   w-det-dow-c01
           else if   w-det-dow-mes        =    04
                     move  07             to   w-det-dow-c01
           else if   w-det-dow-mes        =    05
                     move  02             to   w-det-dow-c01
           else if   w-det-dow-mes        =    06
                     move  05             to   w-det-dow-c01
           else if   w-det-dow-mes        =    07
                     move  07             to   w-det-dow-c01
           else if   w-det-dow-mes        =    08
                     move  03             to   w-det-dow-c01
           else if   w-det-dow-mes        =    09
                     move  06             to   w-det-dow-c01
           else if   w-det-dow-mes        =    10
                     move  01             to   w-det-dow-c01
           else if   w-det-dow-mes        =    11
                     move  04             to   w-det-dow-c01
           else if   w-det-dow-mes        =    12
                     move  06             to   w-det-dow-c01          .
       det-dow-lit-150.
      *                  *---------------------------------------------*
      *                  * Determinazione relativa al giorno           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prima variabile in seconda variabile    *
      *                      *-----------------------------------------*
           move      w-det-dow-c01        to   w-det-dow-c02          .
      *                      *-----------------------------------------*
      *                      * Seconda variabile                       *
      *                      *-----------------------------------------*
           move      w-det-dow-gio        to   w-det-dow-x01          .
       det-dow-lit-160.
           move      w-det-dow-x01        to   w-det-dow-x02          .
           subtract  7                    from w-det-dow-x01          .
           if        w-det-dow-x01        >    zero
                     go to det-dow-lit-160.
           subtract  1                    from w-det-dow-x02          .
           add       w-det-dow-x02        to   w-det-dow-c02          .
           if        w-det-dow-c02        not  > 7
                     go to det-dow-lit-200.
           subtract  7                    from w-det-dow-c02          .
       det-dow-lit-200.
      *                  *---------------------------------------------*
      *                  * Determinazione relativa al secolo - valore  *
      *                  * compreso tra 19 e 20                        *
      *                  *---------------------------------------------*
           if        w-det-dow-sec        =    19
                     move  03             to   w-det-dow-c03
           else if   w-det-dow-sec        =    20
                     move  02             to   w-det-dow-c03
           else if   w-det-dow-sec        =    21
                     move  07             to   w-det-dow-c03
           else if   w-det-dow-sec        =    21
                     move  05             to   w-det-dow-c03          .
       det-dow-lit-300.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione degli anni                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Terza variabile in quarta variabile         *
      *                  *---------------------------------------------*
           move      w-det-dow-c03        to   w-det-dow-c04          .
      *                  *---------------------------------------------*
      *                  * Confronto con gli anni della tabella '0'    *
      *                  *---------------------------------------------*
           if        w-det-dow-ann        =    00   or
                     w-det-dow-ann        =    06   or
                     w-det-dow-ann        =    17   or
                     w-det-dow-ann        =    23   or
                     w-det-dow-ann        =    28   or
                     w-det-dow-ann        =    34   or
                     w-det-dow-ann        =    45   or
                     w-det-dow-ann        =    51   or
                     w-det-dow-ann        =    56   or
                     w-det-dow-ann        =    62   or
                     w-det-dow-ann        =    73   or
                     w-det-dow-ann        =    79   or
                     w-det-dow-ann        =    84   or
                     w-det-dow-ann        =    90
                     go to det-dow-lit-800.
      *                  *---------------------------------------------*
      *                  * Confronto con gli anni della tabella '1'    *
      *                  *---------------------------------------------*
           if        w-det-dow-ann        =    01   or
                     w-det-dow-ann        =    07   or
                     w-det-dow-ann        =    12   or
                     w-det-dow-ann        =    18   or
                     w-det-dow-ann        =    29   or
                     w-det-dow-ann        =    35   or
                     w-det-dow-ann        =    40   or
                     w-det-dow-ann        =    46   or
                     w-det-dow-ann        =    57   or
                     w-det-dow-ann        =    63   or
                     w-det-dow-ann        =    68   or
                     w-det-dow-ann        =    74   or
                     w-det-dow-ann        =    85   or
                     w-det-dow-ann        =    91   or
                     w-det-dow-ann        =    96
                     add  1               to   w-det-dow-c04
                     go to det-dow-lit-800.
      *                  *---------------------------------------------*
      *                  * Confronto con gli anni della tabella '2'    *
      *                  *---------------------------------------------*
           if        w-det-dow-ann        =    02   or
                     w-det-dow-ann        =    13   or
                     w-det-dow-ann        =    19   or
                     w-det-dow-ann        =    24   or
                     w-det-dow-ann        =    30   or
                     w-det-dow-ann        =    41   or
                     w-det-dow-ann        =    47   or
                     w-det-dow-ann        =    52   or
                     w-det-dow-ann        =    58   or
                     w-det-dow-ann        =    69   or
                     w-det-dow-ann        =    75   or
                     w-det-dow-ann        =    80   or
                     w-det-dow-ann        =    86   or
                     w-det-dow-ann        =    97
                     add  2               to   w-det-dow-c04
                     go to det-dow-lit-800.
      *                  *---------------------------------------------*
      *                  * Confronto con gli anni della tabella '3'    *
      *                  *---------------------------------------------*
           if        w-det-dow-ann        =    03   or
                     w-det-dow-ann        =    08   or
                     w-det-dow-ann        =    14   or
                     w-det-dow-ann        =    25   or
                     w-det-dow-ann        =    31   or
                     w-det-dow-ann        =    36   or
                     w-det-dow-ann        =    42   or
                     w-det-dow-ann        =    53   or
                     w-det-dow-ann        =    59   or
                     w-det-dow-ann        =    64   or
                     w-det-dow-ann        =    70   or
                     w-det-dow-ann        =    81   or
                     w-det-dow-ann        =    87   or
                     w-det-dow-ann        =    92   or
                     w-det-dow-ann        =    98
                     add  3               to   w-det-dow-c04
                     go to det-dow-lit-800.
      *                  *---------------------------------------------*
      *                  * Confronto con gli anni della tabella '4'    *
      *                  *---------------------------------------------*
           if        w-det-dow-ann        =    09   or
                     w-det-dow-ann        =    15   or
                     w-det-dow-ann        =    20   or
                     w-det-dow-ann        =    26   or
                     w-det-dow-ann        =    37   or
                     w-det-dow-ann        =    43   or
                     w-det-dow-ann        =    48   or
                     w-det-dow-ann        =    54   or
                     w-det-dow-ann        =    65   or
                     w-det-dow-ann        =    71   or
                     w-det-dow-ann        =    76   or
                     w-det-dow-ann        =    82   or
                     w-det-dow-ann        =    93   or
                     w-det-dow-ann        =    99
                     add  4               to   w-det-dow-c04
                     go to det-dow-lit-800.
      *                  *---------------------------------------------*
      *                  * Confronto con gli anni della tabella '5'    *
      *                  *---------------------------------------------*
           if        w-det-dow-ann        =    04   or
                     w-det-dow-ann        =    10   or
                     w-det-dow-ann        =    21   or
                     w-det-dow-ann        =    27   or
                     w-det-dow-ann        =    32   or
                     w-det-dow-ann        =    38   or
                     w-det-dow-ann        =    49   or
                     w-det-dow-ann        =    55   or
                     w-det-dow-ann        =    60   or
                     w-det-dow-ann        =    66   or
                     w-det-dow-ann        =    77   or
                     w-det-dow-ann        =    83   or
                     w-det-dow-ann        =    88   or
                     w-det-dow-ann        =    94
                     add  5               to   w-det-dow-c04
                     go to det-dow-lit-800.
      *                  *---------------------------------------------*
      *                  * Confronto con gli anni della tabella '6'    *
      *                  *---------------------------------------------*
           if        w-det-dow-ann        =    05   or
                     w-det-dow-ann        =    11   or
                     w-det-dow-ann        =    16   or
                     w-det-dow-ann        =    22   or
                     w-det-dow-ann        =    33   or
                     w-det-dow-ann        =    39   or
                     w-det-dow-ann        =    44   or
                     w-det-dow-ann        =    50   or
                     w-det-dow-ann        =    61   or
                     w-det-dow-ann        =    67   or
                     w-det-dow-ann        =    72   or
                     w-det-dow-ann        =    78   or
                     w-det-dow-ann        =    89   or
                     w-det-dow-ann        =    95
                     add  6               to   w-det-dow-c04
                     go to det-dow-lit-800.
       det-dow-lit-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione quarta variabile            *
      *                  *---------------------------------------------*
           if        w-det-dow-c04        not  > 7
                     go to det-dow-lit-820.
           subtract  7                    from w-det-dow-c04          .
       det-dow-lit-820.
      *                  *---------------------------------------------*
      *                  * Preparazione quinta variabile               *
      *                  *---------------------------------------------*
           if        w-det-dow-c02        =    1
                     move  6              to   w-det-dow-c05
           else if   w-det-dow-c02        =    2
                     move  7              to   w-det-dow-c05
           else if   w-det-dow-c02        =    3
                     move  1              to   w-det-dow-c05
           else if   w-det-dow-c02        =    4
                     move  2              to   w-det-dow-c05
           else if   w-det-dow-c02        =    5
                     move  3              to   w-det-dow-c05
           else if   w-det-dow-c02        =    6
                     move  4              to   w-det-dow-c05
           else if   w-det-dow-c02        =    7
                     move  5              to   w-det-dow-c05          .
      *                  *---------------------------------------------*
      *                  * Preparazione sesta variabile                *
      *                  *---------------------------------------------*
           move      w-det-dow-c05        to   w-det-dow-c06          .
           subtract  1                    from w-det-dow-c04          .
           add       w-det-dow-c04        to   w-det-dow-c06          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione sesta variabile             *
      *                  *---------------------------------------------*
           if        w-det-dow-c06        not  > 7
                     go to det-dow-lit-900.
           subtract  7                    from w-det-dow-c06          .
       det-dow-lit-900.
      *              *-------------------------------------------------*
      *              * Valori in uscita                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiustamento per i primi due mesi se l'an- *
      *                  * no e' bisestile                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se si tratta dei primi due mesi    *
      *                      * dell'anno                               *
      *                      *-----------------------------------------*
           if        w-det-dow-mes        >    2
                     go to det-dow-lit-920.
      *                      *-----------------------------------------*
      *                      * Test se si tratta di un anno bisestile  *
      *                      *-----------------------------------------*
           if        w-det-dow-snb        =    spaces
                     go to det-dow-lit-920.
      *                      *-----------------------------------------*
      *                      * Test che la sesta variabile non sia a   *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           if        w-det-dow-c06        =    zero
                     go to det-dow-lit-920.
      *                      *-----------------------------------------*
      *                      * Se la sesta variabile e' a pari a 1     *
      *                      * diventa uguale a 7 altrimenti si sot-   *
      *                      * trae 1                                  *
      *                      *-----------------------------------------*
           if        w-det-dow-c06        =    1
                     move  7              to   w-det-dow-c06
                     go to det-dow-lit-920.
      *                      *-----------------------------------------*
      *                      * Diminuzione di 1 della sesta variabile  *
      *                      *-----------------------------------------*
           subtract  1                    from w-det-dow-c06          .
       det-dow-lit-920.
      *              *-------------------------------------------------*
      *              * Valori determinati                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sesta variabile in valore di uscita         *
      *                  *---------------------------------------------*
           move      w-det-dow-c06        to   w-det-dow-num          .
      *                  *---------------------------------------------*
      *                  * Se il risulatato della routine non e' mag-  *
      *                  * giore di zero, uscita con literal non tro-  *
      *                  * vato                                        *
      *                  *---------------------------------------------*
           if        w-det-dow-num        =    zero
                     move  8              to   w-det-dow-num          .
           move      w-det-dow-tde
                    (w-det-dow-num)       to   w-det-dow-lit          .
       det-dow-lit-999.
           exit.

