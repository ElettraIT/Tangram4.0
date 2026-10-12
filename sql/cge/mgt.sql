/*    *==========================================================*/
/*    * Tabella [mgt] Primanota : testate                        */
/*    *                                                          */
/*    * N.B.: eliminata la data registrazione dalla chiave       */
/*    *                                                          */
/*    * Aggiornamento 18.02.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.nnnnnnnn                     */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di registrazione di contabilita' generale        */
/*       *-------------------------------------------------------*/
  dat_reg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale di contabilita'                        */
/*       *-------------------------------------------------------*/
  cod_cau                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale si/no movimento di bilancio della causale     */
/*       *                                                       */
/*       * - S : Si, movimento di rettifica al bilancio          */
/*       * - N : No, movimento di contabilita' ordinaria         */
/*       * - X : Movimento di chiusura e apertura bilancio       */
/*       *-------------------------------------------------------*/
  snx_mob                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento iva                                    */
/*       *                                                       */
/*       * - 1 : Dare cliente Italia o Estero non CEE            */
/*       * - 2 : Avere cliente Italia o Estero non CEE           */
/*       * - 3 : Corrispettivi Italia o Estero non CEE           */
/*       *                                                       */
/*       * - 4 : Dare fornitore Italia o Estero non CEE          */
/*       * - 5 : Avere fornitore Italia o Estero non CEE         */
/*       * - 6 : Bolla doganale Estero non CEE                   */
/*       *                                                       */
/*       * - A : Dare cliente Estero CEE                         */
/*       * - B : Avere cliente Estero CEE                        */
/*       *                                                       */
/*       * - D : Dare fornitore Estero CEE                       */
/*       * - E : Avere fornitore Estero CEE                      */
/*       *-------------------------------------------------------*/
  tip_iva                   varchar (01)                    null,
/*       *-------------------------------------------------------*/
/*       * Descrizione causale                                   */
/*       *-------------------------------------------------------*/
  des_cau                   text                        not null,
/*       *-------------------------------------------------------*/
/*       * Data documento                                        */
/*       *-------------------------------------------------------*/
  dat_doc                   date                            null,
/*       *-------------------------------------------------------*/
/*       * Numero documento                                      */
/*       *-------------------------------------------------------*/
  num_doc                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice numerazione per giornale iva                   */
/*       *-------------------------------------------------------*/
  cod_num                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo iva                                 */
/*       *-------------------------------------------------------*/
  prt_iva                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di avvenuta stampa libro giornale                */
/*       *-------------------------------------------------------*/
  flg_gio                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
