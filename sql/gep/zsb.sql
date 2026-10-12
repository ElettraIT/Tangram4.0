/*    *==========================================================*/
/*    * Tabella [zsb] Calcolo spese bollo                  {gep} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_pag),
/*       *=======================================================*/
/*       * Tipo di pagamento                                     */
/*       *                                                       */
/*       * - 09 : Ricevuta bancaria tradizionale                 */
/*       * - 10 : Tratta                                         */
/*       * - 11 : Paghero'                                       */
/*       *-------------------------------------------------------*/
  tip_pag                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di automatismo per la determinazione delle spese */
/*       * bollo per questo tipo pagamento                       */
/*       *                                                       */
/*       * - 01 : In percentuale sul totale scadenza             */
/*       * - 02 : A scaglioni                                    */
/*       * - 03 : A scaglioni con percentuale sull'eccedenza     */
/*       *                                                       */
/*       * Solo se tipo pagamento diverso da 00                  */
/*       *-------------------------------------------------------*/
  tau_spb                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Percentuale per la determinazione delle spese bollo   */
/*       * per questo tipo di pagamento                          */
/*       *                                                       */
/*       * Solo se tipo pagamento diverso da 00                  */
/*       *                                                       */
/*       * Solo se tipo automatismo 01                           */
/*       *-------------------------------------------------------*/
  per_spb                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Tetto di esenzione per la spesa                       */
/*       *                                                       */
/*       * Solo se tipo pagamento diverso da 00                  */
/*       *                                                       */
/*       * Per tutti i tipi di automatismo                       */
/*       *-------------------------------------------------------*/
  tet_spb                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Valore minimo delle spese bollo per questo tipo di    */
/*       * pagamento                                             */
/*       *                                                       */
/*       * Solo se tipo pagamento diverso da 00                  */
/*       *                                                       */
/*       * Per tutti i tipi di automatismo                       */
/*       *-------------------------------------------------------*/
  min_spb                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Valore massimo delle spese bollo per questo tipo di   */
/*       * pagamento                                             */
/*       *                                                       */
/*       * Solo se tipo pagamento diverso da 00                  */
/*       *                                                       */
/*       * Per tutti i tipi di automatismo                       */
/*       *-------------------------------------------------------*/
  max_spb                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di arrotondamento sul valore determinato delle   */
/*       * spese bollo per questo tipo di pagamento              */
/*       *                                                       */
/*       * - 00 : Nessun arrotondamento                          */
/*       * - 01 : Arrotondamento al valore superiore             */
/*       * - 02 : Arrotondamento al valore inferiore             */
/*       * - 03 : Arrotondamento di tipo commerciale             */
/*       * - 04 : Arrotondamento di tipo statale                 */
/*       *                                                       */
/*       * Solo se tipo pagamento diverso da 00                  */
/*       *                                                       */
/*       * Per tutti i tipi di automatismo                       */
/*       *-------------------------------------------------------*/
  tar_spb                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Valore di arrotondamento sul valore determinato delle */
/*       * spese bollo per questo tipo di pagamento              */
/*       *                                                       */
/*       * Solo se tipo pagamento diverso da 00                  */
/*       *                                                       */
/*       * Solo se tipo arrotondamento diverso da 00             */
/*       *                                                       */
/*       * Per tutti i tipi di automatismo                       */
/*       *-------------------------------------------------------*/
  var_spb                   bigint  (09)                not null
