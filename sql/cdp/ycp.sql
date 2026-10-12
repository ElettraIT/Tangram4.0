/*    *==========================================================*/
/*    * Tabella [ycp] Tipi movimento commesse produzione   {cdp} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_tcp),
/*       *=======================================================*/
/*       * Codice tipo movimento                                 */
/*       *-------------------------------------------------------*/
  cod_tcp                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo movimento in uppercase               */
/*       *-------------------------------------------------------*/
  des_key                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo movimento                            */
/*       *-------------------------------------------------------*/
  des_tcp                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Password                                              */
/*       *-------------------------------------------------------*/
  pwd_tcp                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice responsabile                                   */
/*       *-------------------------------------------------------*/
  rsp_tcp                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Validita' rispetto alle dipendenze                    */
/*       *                                                       */
/*       * - 01 : Documento valido per tutte le dipendenze       */
/*       * - 02 : Documento valido per una sola dipendenza       */
/*       *-------------------------------------------------------*/
  vld_dpz                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza per cui il documento e' valido      */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo origine del documento associato al tipo movimen- */
/*       * to commessa di produzione                             */
/*       *                                                       */
/*       *   - 01 : Manuale                                      */
/*       *   - 11 : Automatica                                   */
/*       *-------------------------------------------------------*/
  org_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo provenienza del documento se origine automatica  */
/*       * - Se tipo origine del documento 11                    */
/*       *   - 01 : Emissione automatica da ordini di produzione */
/*       *-------------------------------------------------------*/
  prv_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla numerazione per il documento associato al tipo  */
/*       * movimento                                             */
/*       *-------------------------------------------------------*/
  sgl_num                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo documento associato al tipo movimen- */
/*       * to per la stampa                                      */
/*       *-------------------------------------------------------*/
  des_stp                   varchar (30)                not null,
/*       *-------------------------------------------------------*/
/*       * Default modulo da stampare                            */
/*       *-------------------------------------------------------*/
  def_mds                   varchar (10)                not null
