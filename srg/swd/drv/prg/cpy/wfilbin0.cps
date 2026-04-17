      *    *===========================================================*
      *    * Subroutines per il trattamento, solo in output, del file  *
      *    * binario [bin].                                            *
      *    * --------------------------------------------------------- *
      *    * Open                                                      *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : - w-fil-bin-pat-bin = Pathname per il file       *
      *    *                                                           *
      *    * Output : - nessuno                                        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       fil-bin-opn-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione del pathname da utilizzare       *
      *              *-------------------------------------------------*
           move      w-fil-bin-pat-bin    to   f-bin-pat              .
      *              *-------------------------------------------------*
      *              * Preparazione del pathname per [b01]             *
      *              *-------------------------------------------------*
           move      f-bin-pat            to   f-b01-pat              .
      *              *-------------------------------------------------*
      *              * Preparazione del pathname per [b02]             *
      *              *-------------------------------------------------*
           move      f-bin-pat            to   f-b02-pat              .
       fil-bin-opn-100.
      *              *-------------------------------------------------*
      *              * Numero di caratteri in sospeso nel buffer di    *
      *              * transito : zero                                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-fil-bin-bdt-ncs      .
      *              *-------------------------------------------------*
      *              * Buffer di transito : a spaces                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-fil-bin-bdt-rec      .
       fil-bin-opn-200.
      *              *-------------------------------------------------*
      *              * Open Output file [b01]                          *
      *              *-------------------------------------------------*
           open      output b01                                       .
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        f-b01-sts            not  = e-not-err
                     move  f-b01-sts      to   w-err
                     move  f-b01-sts      to   p-rsc
                     go to fil-bin-opn-999.
       fil-bin-opn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     fil-bin-opn-999.
       fil-bin-opn-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per il trattamento, solo in output, del file  *
      *    * binario [bin].                                            *
      *    * --------------------------------------------------------- *
      *    * Close                                                     *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : - nessuno                                        *
      *    *                                                           *
      *    * Output : - nessuno                                        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       fil-bin-cls-000.
      *              *-------------------------------------------------*
      *              * Test se status attuale ad errore                *
      *              *-------------------------------------------------*
           if        w-err                not  = spaces
                     go to fil-bin-cls-999.
      *              *-------------------------------------------------*
      *              * Close file [b01]                                *
      *              *-------------------------------------------------*
           close     b01                                              .
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        f-b01-sts            not  = e-not-err
                     move  f-b01-sts      to   w-err
                     move  f-b01-sts      to   p-rsc
                     go to fil-bin-cls-999.
      *              *-------------------------------------------------*
      *              * Se numero di caratteri in sospeso nel buffer di *
      *              * transito a zero : ad uscita                     *
      *              *-------------------------------------------------*
           if        w-fil-bin-bdt-ncs    =    zero
                     go to fil-bin-cls-900.
       fil-bin-cls-100.
      *              *-------------------------------------------------*
      *              * Open Extend file [b02]                          *
      *              *-------------------------------------------------*
           open      extend b02                                       .
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        f-b02-sts            not  = e-not-err
                     move  f-b02-sts      to   w-err
                     move  f-b02-sts      to   p-rsc
                     go to fil-bin-cls-999.
       fil-bin-cls-200.
      *              *-------------------------------------------------*
      *              * Indice per i caratteri in sospeso nel buffer di *
      *              * transito : a zero                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-fil-bin-inx-i01      .
       fil-bin-cls-300.
      *              *-------------------------------------------------*
      *              * Incremento indice per i caratteri in sospeso    *
      *              * nel buffer di transito                          *
      *              *-------------------------------------------------*
           add       1                    to   w-fil-bin-inx-i01      .
      *              *-------------------------------------------------*
      *              * Se oltre il numero di caratteri in sospeso nel  *
      *              * buffer di transito : a chiusura                 *
      *              *-------------------------------------------------*
           if        w-fil-bin-inx-i01    >    w-fil-bin-bdt-ncs
                     go to fil-bin-cls-500.
       fil-bin-cls-400.
      *              *-------------------------------------------------*
      *              * Preparazione record di [b02] con il carattere   *
      *              * indicato dall'indice di scansione               *
      *              *-------------------------------------------------*
           move      w-fil-bin-bdt-chr
                    (w-fil-bin-inx-i01)   to   b02-chr                .
      *              *-------------------------------------------------*
      *              * Scrittura record di [b02] per il carattere in-  *
      *              * dicato dall'indice di scansione                 *
      *              *-------------------------------------------------*
           write     b02-rec                                          .
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        f-b02-sts            not  = e-not-err
                     move  f-b02-sts      to   w-err
                     move  f-b02-sts      to   p-rsc
                     go to fil-bin-cls-999.
      *              *-------------------------------------------------*
      *              * Riciclo sul carattere successivo nel buffer di  *
      *              * transito                                        *
      *              *-------------------------------------------------*
           go to     fil-bin-cls-300.
       fil-bin-cls-500.
      *              *-------------------------------------------------*
      *              * Close file [b02]                                *
      *              *-------------------------------------------------*
           close     b02                                              .
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        f-b02-sts            not  = e-not-err
                     move  f-b02-sts      to   w-err
                     move  f-b02-sts      to   p-rsc
                     go to fil-bin-cls-999.
       fil-bin-cls-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     fil-bin-cls-999.
       fil-bin-cls-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per il trattamento, solo in output, del file  *
      *    * binario [bin].                                            *
      *    * --------------------------------------------------------- *
      *    * Delete                                                    *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : - nessuno                                        *
      *    *                                                           *
      *    * Output : - nessuno                                        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       fil-bin-dlt-000.
      *              *-------------------------------------------------*
      *              * Test se status attuale ad errore                *
      *              *-------------------------------------------------*
           if        w-err                not  = spaces
                     go to fil-bin-dlt-999.
      *              *-------------------------------------------------*
      *              * Delete                                          *
      *              *-------------------------------------------------*
           delete    file   bin                                       .
       fil-bin-dlt-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per il trattamento, solo in output, del file  *
      *    * binario [bin].                                            *
      *    * --------------------------------------------------------- *
      *    * Write                                                     *
      *    * --------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : - w-fil-bin-wrt-lun = Lunghezza del record da    *
      *    *                                scrivere                   *
      *    *                                                           *
      *    *                                - Max 8192 caratteri       *
      *    *                                                           *
      *    *                                - Se pari a zero viene e-  *
      *    *                                  seguita la scrittura del *
      *    *                                  record con eliminazione  *
      *    *                                  degli spazi in coda, ed  *
      *    *                                  in questo caso un even-  *
      *    *                                  tuale record a spaces    *
      *    *                                  non verrebbe assoluta-   *
      *    *                                  mente scritto            *
      *    *                                                           *
      *    *          - w-fil-bin-wrt-rec = Record da scrivere         *
      *    *                                                           *
      *    * Output : - nessuno                                        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       fil-bin-wrt-000.
      *              *-------------------------------------------------*
      *              * Test se status attuale ad errore                *
      *              *-------------------------------------------------*
           if        w-err                not  = spaces
                     go to fil-bin-wrt-999.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda della lunghezza record da  *
      *              * scrivere richiesta, con eventuale riduzione a   *
      *              * 8192 della lunghezza record da scrivere         *
      *              *-------------------------------------------------*
           if        w-fil-bin-wrt-lun    =    zero
                     go to fil-bin-wrt-100
           else if   w-fil-bin-wrt-lun    >    8192
                     move  8192           to   w-fil-bin-wrt-lun
                     go to fil-bin-wrt-200
           else      go to fil-bin-wrt-200.
       fil-bin-wrt-100.
      *              *-------------------------------------------------*
      *              * Se richiesta la scrittura con lunghezza zero    *
      *              *-------------------------------------------------*
       fil-bin-wrt-110.
      *                  *---------------------------------------------*
      *                  * Determinazione della lunghezza effettiva se *
      *                  * si eliminano gli spazi in coda              *
      *                  *---------------------------------------------*
           move      zero                 to   w-fil-bin-ctr-c01      .
           inspect   w-fil-bin-wrt-rec
                                      tallying w-fil-bin-ctr-c01
                                  for trailing spaces                 .
           move      8192                 to   w-fil-bin-wrt-lun      .
           subtract  w-fil-bin-ctr-c01    from w-fil-bin-wrt-lun      .
       fil-bin-wrt-120.
      *                  *---------------------------------------------*
      *                  * Se la lunghezza effettiva e' pari a zero,   *
      *                  * cioe' se il record e' interamente composto  *
      *                  * di spazi : ad uscita senza alcuna azione    *
      *                  *---------------------------------------------*
           if        w-fil-bin-wrt-lun    =    zero
                     go to fil-bin-wrt-900.
       fil-bin-wrt-200.
      *              *-------------------------------------------------*
      *              * Se richiesta la scrittura con lunghezza maggio- *
      *              * re di zero                                      *
      *              *-------------------------------------------------*
       fil-bin-wrt-210.
      *                  *---------------------------------------------*
      *                  * Determinazione indice per carattere inizia- *
      *                  * le nel buffer di transito, pari al numero   *
      *                  * di caratteri in sospeso nel buffer aumenta- *
      *                  * to di 1                                     *
      *                  *---------------------------------------------*
           move      w-fil-bin-bdt-ncs    to   w-fil-bin-inx-i01      .
           add       1                    to   w-fil-bin-inx-i01      .
      *                  *---------------------------------------------*
      *                  * Determinazione indice per carattere finale  *
      *                  * nel buffer di transito, pari al numero di   *
      *                  * caratteri in sospeso nel buffer aumentato   *
      *                  * della lunghezza record da scrivere          *
      *                  *---------------------------------------------*
           move      w-fil-bin-bdt-ncs    to   w-fil-bin-inx-i02      .
           add       w-fil-bin-wrt-lun    to   w-fil-bin-inx-i02      .
       fil-bin-wrt-220.
      *                  *---------------------------------------------*
      *                  * Test per vedere se l'indice per il caratte- *
      *                  * re finale supera la capacita' del buffer,   *
      *                  * con deviazione in conseguenza               *
      *                  *---------------------------------------------*
           if        w-fil-bin-inx-i02    >    8192
                     go to fil-bin-wrt-400.
       fil-bin-wrt-300.
      *                  *---------------------------------------------*
      *                  * Se l'indice per il carattere finale rientra *
      *                  * nei limiti della capacita' del buffer       *
      *                  *---------------------------------------------*
       fil-bin-wrt-310.
      *                      *-----------------------------------------*
      *                      * Determinazione del numero di caratteri  *
      *                      * da spostare, come differenza tra indice *
      *                      * iniziale ed indice finale, il tutto au- *
      *                      * mentato di 1                            *
      *                      *-----------------------------------------*
           move      w-fil-bin-inx-i02    to   w-fil-bin-ctr-c01      .
           subtract  w-fil-bin-inx-i01    from w-fil-bin-ctr-c01      .
           add       1                    to   w-fil-bin-ctr-c01      .
      *                      *-----------------------------------------*
      *                      * Spostamento effettivo nel buffer        *
      *                      *-----------------------------------------*
           move      w-fil-bin-wrt-rec    to  w-fil-bin-bdt-rec
                                              (w-fil-bin-inx-i01 :
                                               w-fil-bin-ctr-c01 )    .
      *                      *-----------------------------------------*
      *                      * Incremento numero di caratteri in so-   *
      *                      * speso nel buffer, per il numero di ca-  *
      *                      * ratteri spostati nel buffer di transito *
      *                      *-----------------------------------------*
           add       w-fil-bin-ctr-c01    to   w-fil-bin-bdt-ncs      .
       fil-bin-wrt-320.
      *                      *-----------------------------------------*
      *                      * Se il buffer di transito non e' ancora  *
      *                      * saturo : se va' ad uscita               *
      *                      *-----------------------------------------*
           if        w-fil-bin-bdt-ncs    <    8192
                     go to fil-bin-wrt-900.
       fil-bin-wrt-330.
      *                      *-----------------------------------------*
      *                      * Se il buffer di transito e' saturo      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Scrittura record di [b01] per l'in- *
      *                          * tero buffer di transito             *
      *                          *-------------------------------------*
           move      w-fil-bin-bdt-rec    to   b01-rec                .
           write     b01-rec                                          .
      *                          *-------------------------------------*
      *                          * Test se errori                      *
      *                          *-------------------------------------*
           if        f-b01-sts            not  = e-not-err
                     move  f-b01-sts      to   w-err
                     move  f-b01-sts      to   p-rsc
                     go to fil-bin-wrt-999.
      *                          *-------------------------------------*
      *                          * Reinizializzazione a zero per il    *
      *                          * numero di caratteri in sospeso nel  *
      *                          * buffer di transito                  *
      *                          *-------------------------------------*
           move      zero                 to   w-fil-bin-bdt-ncs      .
      *                          *-------------------------------------*
      *                          * Reinizializzazione a spaces per il  *
      *                          * buffer di transito                  *
      *                          *-------------------------------------*
           move      spaces               to   w-fil-bin-bdt-rec      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     fil-bin-wrt-900.
       fil-bin-wrt-400.
      *                  *---------------------------------------------*
      *                  * Se l'indice per il carattere finale supera  *
      *                  * i limiti della capacita' del buffer         *
      *                  *---------------------------------------------*
       fil-bin-wrt-410.
      *                      *-----------------------------------------*
      *                      * Determinazione del numero di caratteri  *
      *                      * da spostare per ottenere la saturazione *
      *                      * del buffer di transito                  *
      *                      *-----------------------------------------*
           move      8192                 to   w-fil-bin-ctr-c01      .
           subtract  w-fil-bin-inx-i01    from w-fil-bin-ctr-c01      .
           add       1                    to   w-fil-bin-ctr-c01      .
      *                      *-----------------------------------------*
      *                      * Spostamento effettivo nel buffer        *
      *                      *-----------------------------------------*
           move      w-fil-bin-wrt-rec    to   w-fil-bin-bdt-rec
                                              (w-fil-bin-inx-i01 :
                                               w-fil-bin-ctr-c01 )    .
      *                      *-----------------------------------------*
      *                      * Scrittura record di [b01] per l'intero  *
      *                      * buffer di transito                      *
      *                      *-----------------------------------------*
           move      w-fil-bin-bdt-rec    to   b01-rec                .
           write     b01-rec                                          .
      *                      *-----------------------------------------*
      *                      * Test se errori                          *
      *                      *-----------------------------------------*
           if        f-b01-sts            not  = e-not-err
                     move  f-b01-sts      to   w-err
                     move  f-b01-sts      to   p-rsc
                     go to fil-bin-wrt-999.
      *                      *-----------------------------------------*
      *                      * Reinizializzazione a zero per il nume-  *
      *                      * ro di caratteri in sospeso nel buffer   *
      *                      * di transito                             *
      *                      *-----------------------------------------*
           move      zero                 to   w-fil-bin-bdt-ncs      .
      *                      *-----------------------------------------*
      *                      * Reinizializzazione a spaces per il buf- *
      *                      * fer di transito                         *
      *                      *-----------------------------------------*
           move      spaces               to   w-fil-bin-bdt-rec      .
       fil-bin-wrt-420.
      *                      *-----------------------------------------*
      *                      * Determinazione dell'indice nel record   *
      *                      * passato come parametro per il trasfe-   *
      *                      * rimento dei caratteri residui           *
      *                      *-----------------------------------------*
           move      w-fil-bin-ctr-c01    to   w-fil-bin-inx-i03      .
           add       1                    to   w-fil-bin-inx-i03      .
      *                      *-----------------------------------------*
      *                      * Determinazione del numero residuo di    *
      *                      * caratteri da trasferire                 *
      *                      *-----------------------------------------*
           move      w-fil-bin-inx-i02    to   w-fil-bin-inx-i01      .
           subtract  8192                 from w-fil-bin-inx-i01      .
      *                      *-----------------------------------------*
      *                      * Spostamento effettivo nel buffer di     *
      *                      * transito dei caratteri residui non an-  *
      *                      * cora scritti                            *
      *                      *-----------------------------------------*
           move      w-fil-bin-wrt-rec
                    (w-fil-bin-inx-i03 :
                     w-fil-bin-inx-i01 )  to   w-fil-bin-bdt-rec
                                              (1 : w-fil-bin-inx-i01) .
      *                      *-----------------------------------------*
      *                      * Incremento numero di caratteri in so-   *
      *                      * speso nel buffer, per il numero di ca-  *
      *                      * ratteri spostati nel buffer di transito *
      *                      *-----------------------------------------*
           add       w-fil-bin-inx-i01    to   w-fil-bin-bdt-ncs      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     fil-bin-wrt-900.
       fil-bin-wrt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     fil-bin-wrt-999.
       fil-bin-wrt-999.
           exit.
