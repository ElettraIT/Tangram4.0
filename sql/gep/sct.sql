/*    *==========================================================*/
/*    * Tabella [sct] Solleciti : testate                  {gep} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cli,
                                                        num_slc),
/*       *=======================================================*/
/*       * Codice cliente                                        */
/*       *-------------------------------------------------------*/
  cod_cli                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero sollecito del cliente                          */
/*       *-------------------------------------------------------*/
  num_slc                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza del cliente                         */
/*       *-------------------------------------------------------*/
  dpz_cli                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro sollecito                                     */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale                               */
/*       * - 03 : alla dipendenza                                */
/*       *-------------------------------------------------------*/
  inl_slc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice agente                                         */
/*       *-------------------------------------------------------*/
  cod_age                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza del cliente per rottura sollecito   */
/*       *                                                       */
/*       * - Spaces : se inoltro sollecito alla Sede o alla Sede */
/*       *            Legale del cliente                         */
/*       * - XXXX   : codice dipendenza del cliente se inoltro   */
/*       *            sollecito alla Dipendenza del cliente      */
/*       *-------------------------------------------------------*/
  dpc_key                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Livello di sollecito da inviare al cliente            */
/*       *-------------------------------------------------------*/
  liv_slc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice tipo di sollecito da inviare al cliente        */
/*       *-------------------------------------------------------*/
  cod_tso                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di stampa sollecito                              */
/*       * - Spaces : Sollecito non stampato                     */
/*       * - 'S'    : Sollecito stampato                         */
/*       *-------------------------------------------------------*/
  flg_stp                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
