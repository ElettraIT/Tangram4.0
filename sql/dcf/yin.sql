/*    *==========================================================*/
/*    * Tabella [yin] Spese incasso fornitori              {dcf} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_spi,
                                                        tip_pag),
/*       *=======================================================*/
/*       * Codice spesa incasso                                  */
/*       *-------------------------------------------------------*/
  cod_spi                   varchar (03)                not null,
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
/*       *        il codice spesa incasso, e deve essere creato  */
/*       *        preventivamente rispetto ai tipi di pagamento  */
/*       *        reali.                                         */
/*       *-------------------------------------------------------*/
  tip_pag                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per il codice spesa incasso, ad uso in-   */
/*       * terno                                                 */
/*       *                                                       */
/*       * Solo se tipo pagamento 00                             */
/*       *-------------------------------------------------------*/
  des_spi                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice iva per spese incasso                          */
/*       *                                                       */
/*       * N.B. : Puo' essere a zero, e in questo caso l'ammon-  */
/*       *        tare delle spese incasso sara' distribuito     */
/*       *        proporzionalmente tra tutti gli imponibili     */
/*       *        presenti nel documento.                        */
/*       *                                                       */
/*       * Solo se tipo pagamento 00                             */
/*       *-------------------------------------------------------*/
  civ_spi                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita per spese incasso                */
/*       *                                                       */
/*       * N.B. : Puo' essere a zero, e in questo caso l'ammon-  */
/*       *        tare delle spese incasso sara' distribuito     */
/*       *        proporzionalmente tra tutte le contropartite   */
/*       *        presenti nel documento.                        */
/*       *                                                       */
/*       * Solo se tipo pagamento 00                             */
/*       *-------------------------------------------------------*/
  ccp_spi                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo funzionamento per il codice spese incasso        */
/*       *                                                       */
/*       * - 1 : Automatico, in funzione del numero di scadenze  */
/*       *       e del tipo di ogni scadenza                     */
/*       * - 2 : Manuale da impostazione ammontare spese         */
/*       *                                                       */
/*       * Solo se tipo pagamento diverso da 00                  */
/*       *-------------------------------------------------------*/
  tfu_spi                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare addebito spesa per ogni scadenza            */
/*       *                                                       */
/*       * Solo se tipo pagamento diverso da 00                  */
/*       *-------------------------------------------------------*/
  amm_spi                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Percentuale per ogni scadenza                         */
/*       *                                                       */
/*       * Solo se tipo pagamento diverso da 00                  */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito, e puo' servire    */
/*       *        solo per applicazioni speciali. Attualmente    */
/*       *        viene forzato al valore normalizzato.          */
/*       *-------------------------------------------------------*/
  per_spi                   decimal (05,02)             not null
