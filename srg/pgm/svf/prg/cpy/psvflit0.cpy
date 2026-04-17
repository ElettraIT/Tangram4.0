      *    *===========================================================*
      *    * Subroutine per le determinazione literal periodo, del ti- *
      *    * po di range (mese, bimestre, trimestre, ecc.) ed eventua- *
      *    * le indice (I trimestre, ecc.)                             *
      *    *-----------------------------------------------------------*
       det-lit-tpr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-lit-tpr-lit      .
           move      spaces               to   w-det-lit-tpr-rng      .
           move      zero                 to   w-det-lit-tpr-inx      .
       det-lit-tpr-100.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        w-det-lit-tpr-din    =    zero or
                     w-det-lit-tpr-dfi    =    zero
                     go to det-lit-tpr-900.
       det-lit-tpr-300.
      *              *-------------------------------------------------*
      *              * Test di confronto anno                          *
      *              *-------------------------------------------------*
           if        w-det-lit-tpr-sin    not  = w-det-lit-tpr-sfi
                     go to det-lit-tpr-900.
       det-lit-tpr-400.
      *              *-------------------------------------------------*
      *              * Test su giorni                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data iniziale                               *
      *                  *---------------------------------------------*
           if        w-det-lit-tpr-gin    not  = 01
                     go to det-lit-tpr-900.
      *                  *---------------------------------------------*
      *                  * Data finale                                 *
      *                  *---------------------------------------------*
           move      w-det-lit-tpr-dfi    to   w-det-dat-nrg-dtb      .
           move      01                   to   w-det-dat-nrg-ngi      .
           perform   det-dat-nrg-000      thru det-dat-nrg-999        .
           move      w-det-dat-nrg-dti    to   s-dat                  .
           if        s-gio                not  = 01
                     go to det-lit-tpr-900.
       det-lit-tpr-500.
      *              *-------------------------------------------------*
      *              * Test su mesi                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se stesso mese                         *
      *                  *---------------------------------------------*
           if        w-det-lit-tpr-min    =    w-det-lit-tpr-mfi
                     go to det-lit-tpr-510.
      *                  *---------------------------------------------*
      *                  * Test se anno intero                         *
      *                  *---------------------------------------------*
           if        w-det-lit-tpr-min    =    01 and
                     w-det-lit-tpr-mfi    =    12
                     go to det-lit-tpr-590.
      *                  *---------------------------------------------*
      *                  * Differenza matematica tra mese iniziale e   *
      *                  * mese finale                                 *
      *                  *---------------------------------------------*
           subtract  w-det-lit-tpr-min    from w-det-lit-tpr-mfi
                                        giving w-det-lit-tpr-msw      .
      *                  *---------------------------------------------*
      *                  * Test su tipi periodi previsti               *
      *                  *---------------------------------------------*
           if        w-det-lit-tpr-msw    =    01
                     go to det-lit-tpr-520.
           if        w-det-lit-tpr-msw    =    02
                     go to det-lit-tpr-530.
           if        w-det-lit-tpr-msw    =    03
                     go to det-lit-tpr-540.
           if        w-det-lit-tpr-msw    =    05
                     go to det-lit-tpr-560.
      *                  *---------------------------------------------*
      *                  * In tutti gli altri casi : a range di mesi   *
      *                  *---------------------------------------------*
           go to     det-lit-tpr-600.
       det-lit-tpr-510.
      *                  *---------------------------------------------*
      *                  * Stesso mese                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione tipo range                 *
      *                      *-----------------------------------------*
           move      "M"                  to   w-det-lit-tpr-rng      .
           move      w-det-lit-tpr-min    to   w-det-lit-tpr-inx      .
      *                      *-----------------------------------------*
      *                      * Preparazione sub-literal                *
      *                      *-----------------------------------------*
           move      spaces               to   w-det-lit-tpr-lw1      .
      *                      *-----------------------------------------*
      *                      * Literal del mese, da segreteria         *
      *                      *-----------------------------------------*
           move      "LM"                 to   s-ope                  .
           move      "E"                  to   s-tip                  .
           move      w-det-lit-tpr-min    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-det-lit-tpr-lw2      .
      *                      *-----------------------------------------*
      *                      * Preparazione literal per anno           *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-det-lit-tpr-sin    to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-det-lit-tpr-lw3      .
      *                      *-----------------------------------------*
      *                      * A preparazione literal                  *
      *                      *-----------------------------------------*
           go to     det-lit-tpr-800.
       det-lit-tpr-520.
      *                  *---------------------------------------------*
      *                  * Bimestre                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione tipo range                 *
      *                      *-----------------------------------------*
           move      "B"                  to   w-det-lit-tpr-rng      .
      *                      *-----------------------------------------*
      *                      * Preparazione sub-literal                *
      *                      *-----------------------------------------*
           if        w-det-lit-tpr-min    =    01
                     move  "1."           to   w-det-lit-tpr-lw1
                     move  01             to   w-det-lit-tpr-inx
           else if   w-det-lit-tpr-min    =    03
                     move  "2."           to   w-det-lit-tpr-lw1
                     move  02             to   w-det-lit-tpr-inx
           else if   w-det-lit-tpr-min    =    05
                     move  "3."           to   w-det-lit-tpr-lw1
                     move  03             to   w-det-lit-tpr-inx
           else if   w-det-lit-tpr-min    =    07
                     move  "4."           to   w-det-lit-tpr-lw1
                     move  04             to   w-det-lit-tpr-inx
           else if   w-det-lit-tpr-min    =    09
                     move  "5."           to   w-det-lit-tpr-lw1
                     move  05             to   w-det-lit-tpr-inx
           else if   w-det-lit-tpr-min    =    11
                     move  "6."           to   w-det-lit-tpr-lw1
                     move  06             to   w-det-lit-tpr-inx
           else      go to det-lit-tpr-600.
           move      "bimestre  "         to   w-det-lit-tpr-lw2      .
      *                      *-----------------------------------------*
      *                      * Preparazione literal per anno           *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-det-lit-tpr-sin    to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-det-lit-tpr-lw3      .
      *                      *-----------------------------------------*
      *                      * A preparazione literal                  *
      *                      *-----------------------------------------*
           go to     det-lit-tpr-800.
       det-lit-tpr-530.
      *                  *---------------------------------------------*
      *                  * Trimestre                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione tipo range                 *
      *                      *-----------------------------------------*
           move      "T"                  to   w-det-lit-tpr-rng      .
      *                      *-----------------------------------------*
      *                      * Preparazione sub-literal                *
      *                      *-----------------------------------------*
           if        w-det-lit-tpr-min    =    01
                     move  "1."           to   w-det-lit-tpr-lw1
                     move  01             to   w-det-lit-tpr-inx
           else if   w-det-lit-tpr-min    =    04
                     move  "2."           to   w-det-lit-tpr-lw1
                     move  02             to   w-det-lit-tpr-inx
           else if   w-det-lit-tpr-min    =    07
                     move  "3."           to   w-det-lit-tpr-lw1
                     move  03             to   w-det-lit-tpr-inx
           else if   w-det-lit-tpr-min    =    10
                     move  "4."           to   w-det-lit-tpr-lw1
                     move  04             to   w-det-lit-tpr-inx
           else      go to det-lit-tpr-600.
           move      "trimestre "         to   w-det-lit-tpr-lw2      .
      *                      *-----------------------------------------*
      *                      * Preparazione literal per anno           *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-det-lit-tpr-sin    to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-det-lit-tpr-lw3      .
      *                      *-----------------------------------------*
      *                      * A preparazione literal                  *
      *                      *-----------------------------------------*
           go to     det-lit-tpr-800.
       det-lit-tpr-540.
      *                  *---------------------------------------------*
      *                  * Quadrimestre                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione tipo range                 *
      *                      *-----------------------------------------*
           move      "Q"                  to   w-det-lit-tpr-rng      .
      *                      *-----------------------------------------*
      *                      * Preparazione sub-literal                *
      *                      *-----------------------------------------*
           if        w-det-lit-tpr-min    =    01
                     move  "1."           to   w-det-lit-tpr-lw1
                     move  01             to   w-det-lit-tpr-inx
           else if   w-det-lit-tpr-min    =    05
                     move  "2."           to   w-det-lit-tpr-lw1
                     move  02             to   w-det-lit-tpr-inx
           else if   w-det-lit-tpr-min    =    09
                     move  "3."           to   w-det-lit-tpr-lw1
                     move  03             to   w-det-lit-tpr-inx
           else      go to det-lit-tpr-600.
           move      "quadrim.  "         to   w-det-lit-tpr-lw2      .
      *                      *-----------------------------------------*
      *                      * Preparazione literal per anno           *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-det-lit-tpr-sin    to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-det-lit-tpr-lw3      .
      *                      *-----------------------------------------*
      *                      * A preparazione literal                  *
      *                      *-----------------------------------------*
           go to     det-lit-tpr-800.
       det-lit-tpr-560.
      *                  *---------------------------------------------*
      *                  * Semestre                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione tipo range                 *
      *                      *-----------------------------------------*
           move      "S"                  to   w-det-lit-tpr-rng      .
      *                      *-----------------------------------------*
      *                      * Preparazione sub-literal                *
      *                      *-----------------------------------------*
           if        w-det-lit-tpr-min    =    01
                     move  "1."           to   w-det-lit-tpr-lw1
                     move  01             to   w-det-lit-tpr-inx
           else if   w-det-lit-tpr-min    =    07
                     move  "2."           to   w-det-lit-tpr-lw1
                     move  02             to   w-det-lit-tpr-inx
           else      go to det-lit-tpr-600.
           move      "semestre  "         to   w-det-lit-tpr-lw2      .
      *                      *-----------------------------------------*
      *                      * Preparazione literal per anno           *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-det-lit-tpr-sin    to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-det-lit-tpr-lw3      .
      *                      *-----------------------------------------*
      *                      * A preparazione literal                  *
      *                      *-----------------------------------------*
           go to     det-lit-tpr-800.
       det-lit-tpr-590.
      *                  *---------------------------------------------*
      *                  * Inizio e fine anno                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione tipo range                 *
      *                      *-----------------------------------------*
           move      "A"                  to   w-det-lit-tpr-rng      .
           move      12                   to   w-det-lit-tpr-inx      .
      *                      *-----------------------------------------*
      *                      * Preparazione sub-literal                *
      *                      *-----------------------------------------*
           move      spaces               to   w-det-lit-tpr-lw1      .
           move      "Anno"               to   w-det-lit-tpr-lw2      .
      *                      *-----------------------------------------*
      *                      * Preparazione literal per anno           *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-det-lit-tpr-sin    to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-det-lit-tpr-lw3      .
      *                      *-----------------------------------------*
      *                      * A preparazione literal                  *
      *                      *-----------------------------------------*
           go to     det-lit-tpr-800.
       det-lit-tpr-600.
      *                  *---------------------------------------------*
      *                  * Un range di mesi                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione tipo range                 *
      *                      *-----------------------------------------*
           move      "X"                  to   w-det-lit-tpr-rng      .
           move      w-det-lit-tpr-msw    to   w-det-lit-tpr-inx      .
           add       01                   to   w-det-lit-tpr-inx      .
      *                      *-----------------------------------------*
      *                      * Literal del mese, da segreteria         *
      *                      *-----------------------------------------*
           move      "LM"                 to   s-ope                  .
           move      "A"                  to   s-tip                  .
           move      w-det-lit-tpr-min    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-all-str-cat (1)      .
      *
           move      "LM"                 to   s-ope                  .
           move      "A"                  to   s-tip                  .
           move      w-det-lit-tpr-mfi    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-all-str-cat (3)      .
      *                      *-----------------------------------------*
      *                      * Preparazione sub-literal                *
      *                      *-----------------------------------------*
           move      spaces               to   w-det-lit-tpr-lw1      .
           move      spaces               to   w-det-lit-tpr-lw2      .
           move      10                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "-"                  to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-det-lit-tpr-lw2      .
      *                      *-----------------------------------------*
      *                      * Preparazione literal per anno           *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-det-lit-tpr-sin    to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-det-lit-tpr-lw3      .
      *                      *-----------------------------------------*
      *                      * A preparazione literal                  *
      *                      *-----------------------------------------*
           go to     det-lit-tpr-800.
       det-lit-tpr-800.
      *              *-------------------------------------------------*
      *              * Preparazione literal comune                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Concatenamento                              *
      *                  *---------------------------------------------*
           move      20                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-det-lit-tpr-lw1    to   w-all-str-cat (1)      .
           move      w-det-lit-tpr-lw2    to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      w-det-lit-tpr-lw3    to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-det-lit-tpr-lit      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-lit-tpr-900.
       det-lit-tpr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-lit-tpr-999.
       det-lit-tpr-999.
           exit.

