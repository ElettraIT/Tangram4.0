/*    *==========================================================*/
/*    * Tabella [zpv] Tabella categorie provvigioni              */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_cpv,
                                                        cod_cpv),
/*       *=======================================================*/
/*       * Tipo categoria di provvigione                         */
/*       *                                                       */
/*       * - 01 : Categorie di provvigione legate all'agente     */
/*       * - 02 : Categorie di provvigione legate al prodotto    */
/*       * - 03 : Categorie di provvigione legate al cliente     */
/*       *-------------------------------------------------------*/
  tip_cpv                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice categoria di provvigione                       */
/*       *-------------------------------------------------------*/
  cod_cpv                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione categoria                                 */
/*       *-------------------------------------------------------*/
  des_cpv                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Mnemonico categoria                                   */
/*       *-------------------------------------------------------*/
  mne_cpv                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione categoria in uppercase                    */
/*       *-------------------------------------------------------*/
  des_key                   varchar (40)                not null