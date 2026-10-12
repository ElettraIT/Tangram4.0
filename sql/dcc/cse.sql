/*    *==========================================================*/
/*    * Tabella [cse] Specifiche per Clienti in e-fatture  {dcc} */
/*    *                                                          */
/*    * Aggiornamento 18.06.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cli,
                                                        dpz_cli,
                                                        cod_tag),
/*       *=======================================================*/
/*       * Codice cliente, deve trovare corrispondenza in        */
/*       * anagrafica clienti per contabilita'                   */
/*       *-------------------------------------------------------*/
  cod_cli                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza cliente                             */
/*       *-------------------------------------------------------*/
  dpz_cli                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice TAG relativo alla rappresentazione tabellare   */
/*       * del tracciato XML per l'e-fattura, versione 1.3       */
/*       *                                                       */
/*       * da '1.2' a '2.5'                                      */
/*       *                                                       */
/*       * Esempio di formato: tag <Anagrafica> = '1.4.1.3'      */
/*       *-------------------------------------------------------*/
  cod_tag                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di esposizione TAG                               */
/*       *                                                       */
/*       * - 'S' : Si, tag attivo                                */
/*       * - 'N' : No, tag non attivo                            */
/*       *-------------------------------------------------------*/
  snx_tag                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Parametri accessori TAG                               */
/*       *                                                       */
/*       * N.B.: attualmente gestito solo il campo di testo      */
/*       *-------------------------------------------------------*/
  pma_alf                   varchar (80)                not null,
  pma_num                   decimal (11,03)             not null,
  pma_dat                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Indicazioni per parametri accessori TAG               */
/*       *                                                       */
/*       * N.B.: campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  pma_ind                   integer (03)                not null
