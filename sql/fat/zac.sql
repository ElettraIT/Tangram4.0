/*    *==========================================================*/
/*    * Tabella [zac] Addebiti o commenti fatturazione     {fat} */
/*    *                                                          */
/*    * Aggiornamento 02.10.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_rec,
                                                        cod_aoc),
/*       *=======================================================*/
/*       * Tipo record                                           */
/*       *                                                       */
/*       * 01 : Addebito                                         */
/*       * 02 : Commento                                         */
/*       * 03 : Dicitura per tipo documento                      */
/*       *-------------------------------------------------------*/
  tip_rec                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice addebito o commento o dicitura                 */
/*       *-------------------------------------------------------*/
  cod_aoc                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione                                           */
/*       *-------------------------------------------------------*/
  des_ao1                   varchar (200)               not null,
  des_ao2                   varchar (200)               not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione in uppercase                              */
/*       *-------------------------------------------------------*/
  des_key                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico                                      */
/*       *-------------------------------------------------------*/
  cod_mne                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice iva                                            */
/*       *                                                       */
/*       * N.B.: solo se tipo record 01 (Addebito)               */
/*       *-------------------------------------------------------*/
  cod_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita contabile                        */
/*       *                                                       */
/*       * N.B.: solo se tipo record 01 (Addebito)               */
/*       *-------------------------------------------------------*/
  cod_ctp                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * L'addebito deve essere cumulato in uno dei totali di  */
/*       * suddivisione per le righe, in valuta                  */
/*       *                                                       */
/*       * - 00 : No, nessun cumulo                              */
/*       * - 01 : Totale merce                                   */
/*       * - 02 : Totale servizi                                 */
/*       * - 03 : Totale imballi                                 */
/*       * - 04 : Totale libero 4                                */
/*       * - 05 : Totale libero 5                                */
/*       * - 06 : Totale libero 6                                */
/*       * - 07 : Totale libero 7                                */
/*       * - 08 : Totale libero 8                                */
/*       * - 09 : Totale extra                                   */
/*       *                                                       */
/*       * N.B.: solo se tipo record 01 (Addebito)               */
/*       *                                                       */
/*       * N.B.: attualmente implementato solo in fatturazione   */
/*       *-------------------------------------------------------*/
  tip_tot                   tinyint (02)                not null