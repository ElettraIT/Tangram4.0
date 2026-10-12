/*    *==========================================================*/
/*    * Tabella [dcs] Clienti commerciali                  {dcc} */
/*    *                                                          */
/*    * Estensione ad anagrafica : castelletto spese             */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cli,
                                                        prg_cst),
/*       *=======================================================*/
/*       * Codice cliente                                        */
/*       *-------------------------------------------------------*/
  cod_cli                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo castelletto                               */
/*       *-------------------------------------------------------*/
  prg_cst                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No addebito spese e modalita'                      */
/*       *                                                       */
/*       * - 01 : No addebito                                    */
/*       * - 02 : Si addebito, secondo le modalita' espresse nel */
/*       *        modo di funzionamento per la spesa             */
/*       * - 03 : Si addebito a percentuale, solo se il funzio-  */
/*       *        namento della spesa e' tipo 05                 */
/*       * - 04 : Si addebito ad importo, solo se il funziona-   */
/*       *        mento della spesa e' tipo 05                   */
/*       *                                                       */
/*       * Per i clienti con fatturazione in valuta non si tiene */
/*       * conto dei tipi funzionamento spesa 02 e 04, che ven-  */
/*       * gono rispettivamente considerati come 03 o 05         */
/*       *-------------------------------------------------------*/
  snm_spe                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * % di addebito spesa                                   */
/*       *-------------------------------------------------------*/
  per_spe                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Importo di addebito spesa                             */
/*       *-------------------------------------------------------*/
  imp_spe                   bigint  (13)                not null