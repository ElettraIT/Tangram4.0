/*    *==========================================================*/
/*    * Tabella [cdb] Classificatori di bilancio                 */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_cdb,
                                                        cod_cdb),
/*       *=======================================================*/
/*       * Tipo classificatore : 'A' = Attivita'                 */
/*       *                       'P' = Passivita'                */
/*       *                       'C' = Costi                     */
/*       *                       'R' = Ricavi                    */
/*       *-------------------------------------------------------*/
  tip_cdb                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice classificatore di bilancio                     */
/*       *-------------------------------------------------------*/
  cod_cdb                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione classificatore di bilancio                */
/*       *-------------------------------------------------------*/
  des_cdb                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice per riclassificazione conto economico          */
/*       *                                                       */
/*       * 00 - non significativo (mastri)                       */
/*       * 01 - Vendite                                          */
/*       * 02 - Rettifiche vendite                               */
/*       * 03 - Rimanenze iniziali                               */
/*       * 04 - Acquisti                                         */
/*       * 05 - Rimanenze finali                                 */
/*       * 06 - Costi di gestione                                */
/*       * 07 - Proventi di gestione                             */
/*       * 08 - Oneri extra-gestione                             */
/*       * 09 - Proventi extra-gestione                          */
/*       *-------------------------------------------------------*/
  cod_rcl                   varchar (02)                not null
