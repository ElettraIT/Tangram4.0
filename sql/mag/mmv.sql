/*    *==========================================================*/
/*    * Tabella [mmv] Saldi valore                         {mag} */
/*    *                                                          */
/*    * Vedi    [mmm] per scaglioni mensili                      */
/*    * Vedi    [mml] per scaglioni LIFO                         */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (ann_ese,
                                                        tip_mag,
                                                        num_mag),
/*       *=======================================================*/
/*       * Anno di esercizio                                     */
/*       *-------------------------------------------------------*/
  ann_ese                   integer (04)                not null,
/*       *-------------------------------------------------------*/
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
  num_mag                   integer (09)                not null,
/*       *-------------------------------------------------------*/
/*       * Data costo ultimo ad inizio esercizio                 */
/*       *-------------------------------------------------------*/
  dcu_ies                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Costo ultimo ad inizio esercizio                      */
/*       *-------------------------------------------------------*/
  ucu_ies                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data costo standard ad inizio esercizio               */
/*       *-------------------------------------------------------*/
  dst_ies                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Costo standard ad inizio esercizio                    */
/*       *-------------------------------------------------------*/
  ust_ies                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data costo standard valido per tutto l' esercizio     */
/*       * attuale in mancanza di costi standard specifici       */
/*       * mensili                                               */
/*       *-------------------------------------------------------*/
  dst_gea                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Costo standard valido per tutto l' esercizio attuale  */
/*       * in mancanza di costi standard specifici mensili       */
/*       *-------------------------------------------------------*/
  ust_gea                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Valore normale del bene in ultimo trimestre esercizio */
/*       * attuale                                               */
/*       *-------------------------------------------------------*/
  vnb_gea                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di rivalutazione esercizio attuale               */
/*       *                                                       */
/*       * - Spaces : No                                         */
/*       * - S      : Si                                         */
/*       *-------------------------------------------------------*/
  fdr_gea                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo costi aggiuntivi chiusura esercizio       */
/*       *-------------------------------------------------------*/
  pca_che                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo rettifiche a costi chiusura esercizio     */
/*       *-------------------------------------------------------*/
  prc_che                   bigint  (13)                not null