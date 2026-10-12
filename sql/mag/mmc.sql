/*    *==========================================================*/
/*    * Tabella [mmc] Costo medio continuo                 {mag} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_mag,
                                                        num_mag),
/*       *=======================================================*/
/*       * Tipo codice di magazzino                              */
/*       *                                                       */
/*       * - 01 : Prodotti di vendita                            */
/*       * - 02 : Semilavorati                                   */
/*       * - 03 : Materie prime                                  */
/*       * - 04 : Materiali vari                                 */
/*       *-------------------------------------------------------*/
  tip_mag                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto numerico                              */
/*       *-------------------------------------------------------*/
  num_mag                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data consolidamento                                   */
/*       *-------------------------------------------------------*/
  dat_con                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data consolidamento in formato 'reverse'              */
/*       *                                                       */
/*       * Si ottiene con il seguente calcolo:                   */
/*       * rf-mmc-dat-rev = (99999999-(19000000+rf-mmc-dat-con)) */
/*       *                                                       */
/*       * Se rf-mmc-dat-con vale zero, questo campo viene for-  */
/*       * zato al valore 99999999                               */
/*       *                                                       */
/*       * E' da osservare pertanto che record senza data di ri- */
/*       * levazione cambio avranno qui il valore 99999999       */
/*       *-------------------------------------------------------*/
  dat_rev                   bigint  (08)                not null,
/*       *-------------------------------------------------------*/
/*       * Data ultima rilevazione costo medio continuo          */
/*       *-------------------------------------------------------*/
  dat_ulr                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' in giacenza alla data di consolidamento     */
/*       *-------------------------------------------------------*/
  gia_ddc                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Costo medio continuo alla data di consolidamento      */
/*       *-------------------------------------------------------*/
  cmc_ddc                   bigint  (11)                not null