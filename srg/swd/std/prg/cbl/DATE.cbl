      *    *===========================================================*
      *    * Work-area per operazioni sulle date                       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cpw"                   .



      *    *===========================================================*
      *    * Subroutines per operazioni sulle date                     *
      *    *                                                           *
      *    * 'det-dat-nrg-000/999'                                     *
      *    *                                                           *
      *    * Routines per l'aumento di una data in giorni              *
      *    *                                                           *
      *    * Input  : w-det-dat-nrg-dtb = Data iniziale                *
      *    *                                                           *
      *    *          w-det-dat-nrg-ngi = nr. giorni di incremento     *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-det-dat-nrg-dti = Data incrementata            *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * 'det-nrg-dat-000/999'                                     *
      *    *                                                           *
      *    * Routines per l'aumento di una data in giorni              *
      *    *                                                           *
      *    * Input  : w-det-nrg-dat-dtb = Data iniziale                *
      *    *                                                           *
      *    *          w-det-nrg-dat-ngd = nr. giorni di decremento     *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-det-nrg-dat-dtd = Data decrementata            *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cps"                   .





      *                      *-----------------------------------------*
      *                      * ___ esempio di incremento ____          *
      *                      *-----------------------------------------*
           move      w_data_in_input      to   w-det-dat-nrg-dtb      .
           move      30                   to   w-det-dat-nrg-ngi      .
           perform   det-dat-nrg-000      thru det-dat-nrg-999        .
           move      w-det-dat-nrg-dti    to   w_data_in_output       .


      *                      *-----------------------------------------*
      *                      * ___ esempio di decremento ____          *
      *                      *-----------------------------------------*
           move      w_data_in_input      to   w-det-nrg-dat-dtb      .
           move      30                   to   w-det-nrg-dat-ngd      .
           perform   det-nrg-dat-000      thru det-nrg-dat-999        .
           move      w-det-nrg-dat-dtd    to   w_data_in_output       .

