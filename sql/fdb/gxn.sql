/*    *==========================================================*/
/*    * Tabella [gxn] Nazioni                              {fdb} */
/*    *                                                          */
/*    * Aggiornamento 20.12.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_naz),
/*       *=======================================================*/
/*       * Codice nazione                                        */
/*       *-------------------------------------------------------*/
  cod_naz                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione nazione                                   */
/*       *-------------------------------------------------------*/
  des_naz                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Annotazioni                                           */
/*       *-------------------------------------------------------*/
  not_naz                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice stato estero per Agenzia Entrate               */
/*       *-------------------------------------------------------*/
  cod_ste                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di appartenenza alla CEE                      */
/*       *                                                       */
/*       *  - 0 : No, nazione non appartenente alla CEE          */
/*       *  - 1 : Si, nazione appartenente alla CEE              */
/*       *-------------------------------------------------------*/
  snx_cee                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di Nazione appartenente a Black List fiscale  */
/*       *                                                       */
/*       *  - 0 : No, nazione non appartenente alla Black List   */
/*       *  - 1 : Si, nazione appartenente alla Black List       */
/*       *-------------------------------------------------------*/
  snx_blk                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di Nazione 'preferita'                        */
/*       *                                                       */
/*       *  - 0 : No, nessuna preferenza                         */
/*       *  - 1 : Si, nazione 'preferita'                        */
/*       *-------------------------------------------------------*/
  snx_prf                   tinyint (01)                not null
