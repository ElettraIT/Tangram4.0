      *    *===========================================================*
      *    * Work per trattamento ore e minuti con trasformazione in   *
      *    * centesimi di ora                                          *
      *    *-----------------------------------------------------------*
       01  w-oem-cdo.
      *        *-------------------------------------------------------*
      *        * Ore e minuti espressi in centesimi                    *
      *        *-------------------------------------------------------*
           05  w-oem-cdo-ore-min          pic s9(06)v9(02) trailing
                                                           separate
                                                           character  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione di comodo per il campo precedente       *
      *        *-------------------------------------------------------*
           05  w-oem-cdo-oem-red redefines
               w-oem-cdo-ore-min.
               10  filler                 pic  9(06)                  .
               10  w-oem-cdo-oem-min      pic  9(02)                  .
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di uscita per test sui minuti in sessantesimi    *
      *        *  - Spaces : Ok                                        *
      *        *  - #      : Ko                                        *
      *        *-------------------------------------------------------*
           05  w-oem-cdo-tst-flg          pic  x(01)                  .






      *    *===========================================================*
      *    * Subroutines per trattamento ore e minuti con trasforma-   *
      *    * zione in centesimi di ora                                 *
      *    *-----------------------------------------------------------*
       oem-cdo-cas-000.
      *              *-------------------------------------------------*
      *              * Conversione                                     *
      *              *                                                 *
      *              * - da : Ore/minuti con i minuti espressi in cen- *
      *              *        tesimi di ora                            *
      *              *                                                 *
      *              * - a  : Ore/minuti con i minuti espressi in ses- *
      *              *        santesimi di ora                         *
      *              *                                                 *
      *              *                                                 *
      *              * Input  :                                        *
      *              *                                                 *
      *              *  - w-oem-cdo-ore-min = Ore e minuti espressi in *
      *              *                        centesimi                *
      *              *                                                 *
      *              * Output :                                        *
      *              *                                                 *
      *              *  - w-oem-cdo-ore-min = Ore e minuti espressi in *
      *              *                        sessantesimi             *
      *              *                                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trasformazione minuti in centesimi          *
      *                  *---------------------------------------------*
           multiply  0,60                 by   w-oem-cdo-oem-min
                                                         rounded      .
       oem-cdo-cas-999.
           exit.
       oem-cdo-sac-000.
      *              *-------------------------------------------------*
      *              * Conversione                                     *
      *              *                                                 *
      *              * - da : Ore/minuti con i minuti espressi in ses- *
      *              *        santesimi di ora                         *
      *              *                                                 *
      *              * - a  : Ore/minuti con i minuti espressi in cen- *
      *              *        tesimi di ora                            *
      *              *                                                 *
      *              *                                                 *
      *              * Input  :                                        *
      *              *                                                 *
      *              *  - w-oem-cdo-ore-min = Ore e minuti espressi in *
      *              *                        sessantesimi             *
      *              *                                                 *
      *              * Output :                                        *
      *              *                                                 *
      *              *  - w-oem-cdo-ore-min = Ore e minuti espressi in *
      *              *                        centesimi                *
      *              *                                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trasformazione minuti in sessantesimi       *
      *                  *---------------------------------------------*
           multiply  1,66666              by   w-oem-cdo-oem-min
                                                         rounded      .
       oem-cdo-sac-999.
           exit.
       oem-cdo-tst-000.
      *              *-------------------------------------------------*
      *              * Test su Ore/minuti con i minuti espressi in     *
      *              * sessantesimi                                    *
      *              *                                                 *
      *              *                                                 *
      *              * Input  :                                        *
      *              *                                                 *
      *              *  - w-oem-cdo-ore-min = Ore e minuti espressi in *
      *              *                        sessantesimi             *
      *              *                                                 *
      *              * Output :                                        *
      *              *                                                 *
      *              *  - w-oem-cdo-tst-flg = Flag di uscita           *
      *              *                         - Spaces : Ok           *
      *              *                         - #      : Ko           *
      *              *                                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trasformazione minuti in sessantesimi       *
      *                  *---------------------------------------------*
           if        w-oem-cdo-oem-min    >    59
                     move  "#"            to   w-oem-cdo-tst-flg
           else      move  spaces         to   w-oem-cdo-tst-flg      .
       oem-cdo-tst-999.
           exit.

