/*    *==========================================================*/
/*    * Tabella [ybo] Spese bollo fornitori                {dcf} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_spb,
                                                        tip_pag),
/*       *=======================================================*/
/*       * Codice spesa bollo                                    */
/*       *-------------------------------------------------------*/
  cod_spb                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di pagamento                                     */
/*       *                                                       */
/*       * - 01 : Rimessa diretta                                */
/*       * - 02 : Incasso elettronico                            */
/*       * - 03 : Ri.Ba.                                         */
/*       * - 04 : Di.Pa.                                         */
/*       * - 05 : Mav - Mediante avviso                          */
/*       * - 06 : Rid - Accredito preautorizzato in conto        */
/*       * - 07 : Bonifico bancario                              */
/*       * - 08 : Conto corrente postale                         */
/*       * - 09 : Ricevuta bancaria tradizionale                 */
/*       * - 10 : Tratta                                         */
/*       * - 11 : Paghero'                                       */
/*       *                                                       */
/*       * N.B. : Il tipo pagamento 00 indica l'intestazione per */
/*       *        il codice spesa bollo, e deve essere creato    */
/*       *        preventivamente rispetto ai tipi di pagamento  */
/*       *        reali.                                         */
/*       *-------------------------------------------------------*/
  tip_pag                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per il codice spesa bollo, ad uso interno */
/*       *                                                       */
/*       * Solo se tipo pagamento 00                             */
/*       *-------------------------------------------------------*/
  des_spb                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice iva per spese bollo                            */
/*       *                                                       */
/*       * N.B. : Puo' essere a zero, e in questo caso l'ammon-  */
/*       *        tare delle spese bollo sara' distribuito pro-  */
/*       *        porzionalmente tra tutti gli imponibili  pre-  */
/*       *        senti nel documento.                           */
/*       *                                                       */
/*       * Solo se tipo pagamento 00                             */
/*       *-------------------------------------------------------*/
  civ_spb                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita per spese bollo                  */
/*       *                                                       */
/*       * N.B. : Puo' essere a zero, e in questo caso l'ammon-  */
/*       *        tare delle spese bollo sara' distribuito pro-  */
/*       *        porzionalmente tra tutte le contropartite      */
/*       *        presenti nel documento.                        */
/*       *                                                       */
/*       * Solo se tipo pagamento 00                             */
/*       *-------------------------------------------------------*/
  ccp_spb                   integer (07)                not null,
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
  tet_spb                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Valore minimo delle spese bollo per questo tipo di    */
/*       * pagamento                                             */
/*       *                                                       */
/*       * Solo se tipo pagamento diverso da 00                  */
/*       *                                                       */
/*       * Per tutti i tipi di automatismo                       */
/*       *-------------------------------------------------------*/
  min_spb                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Valore massimo delle spese bollo per questo tipo di   */
/*       * pagamento                                             */
/*       *                                                       */
/*       * Solo se tipo pagamento diverso da 00                  */
/*       *                                                       */
/*       * Per tutti i tipi di automatismo                       */
/*       *-------------------------------------------------------*/
  max_spb                   bigint  (11)                not null,
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
  var_spb                   integer (07)                not null
