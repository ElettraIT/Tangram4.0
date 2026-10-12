/*    *==========================================================*/
/*    * Tabella [osk] Ordini di spedizione                 {ods} */
/*    *                                                          */
/*    * Packing list                                             */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt,
                                                        num_prg,
                                                        num_prc),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo riga                                      */
/*       *-------------------------------------------------------*/
  num_prg                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo collo                              */
/*       *-------------------------------------------------------*/
  num_prc                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Contrassegno per il collo                             */
/*       *                                                       */
/*       * N.B.: Relativo al progressivo riga zero               */
/*       *-------------------------------------------------------*/
  cts_prc                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di collo                                         */
/*       *                                                       */
/*       * N.B.: Relativo al progressivo riga zero               */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  tip_prc                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' di prodotto contenuta nel collo relativa    */
/*       * alla riga ordine di spedizione cui si riferisce       */
/*       * espressa in Kg.                                       */
/*       * N.B.: Relativo al progressivo riga non-zero           */
/*       *-------------------------------------------------------*/
  qta_prc                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Peso lordo espresso in Kg.                            */
/*       *                                                       */
/*       * N.B.: Se relativo al progressivo riga zero, si rife-  */
/*       *       risce al collo per intero                       */
/*       * N.B.: Se relativo al progressivo riga non-zero, si    */
/*       *       riferisce alla singola riga dell'ordine di spe- */
/*       *       dizione, ed il dato e' stato ripreso dai valori */
/*       *       anagrafici                                      */
/*       * N.B.: Campo attualmente gestito solamente per il      */
/*       *       progressivo riga zero                           */
/*       *-------------------------------------------------------*/
  lrd_prc                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Peso tara espresso in Kg.                             */
/*       *                                                       */
/*       * N.B.: Se relativo al progressivo riga zero, si rife-  */
/*       *       risce al collo per intero                       */
/*       * N.B.: Se relativo al progressivo riga non-zero, si    */
/*       *       riferisce alla singola riga dell'ordine di spe- */
/*       *       dizione, ed il dato e' stato ripreso dai valori */
/*       *       anagrafici                                      */
/*       * N.B.: Campo attualmente gestito solamente per il      */
/*       *       progressivo riga zero                           */
/*       *-------------------------------------------------------*/
  tar_prc                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Volume unitario                                       */
/*       *                                                       */
/*       *                                                       */
/*       * N.B.: Se relativo al progressivo riga zero, si rife-  */
/*       *       risce al collo per intero                       */
/*       * N.B.: Se relativo al progressivo riga non-zero, si    */
/*       *       riferisce alla singola riga dell'ordine di spe- */
/*       *       dizione, ed il dato e' stato ripreso dai valori */
/*       *       anagrafici                                      */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  vol_prc                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Dimensioni del collo : Larghezza                      */
/*       *                                                       */
/*       * N.B.: Se relativo al progressivo riga zero, si rife-  */
/*       *       risce al collo per intero                       */
/*       * N.B.: Se relativo al progressivo riga non-zero, si    */
/*       *       riferisce alla singola riga dell'ordine di spe- */
/*       *       dizione, ed il dato e' stato ripreso dai valori */
/*       *       anagrafici                                      */
/*       * N.B.: Campo attualmente gestito solamente per il      */
/*       *       progressivo riga zero                           */
/*       *-------------------------------------------------------*/
  dim_lar                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Dimensioni del collo : Altezza                        */
/*       *                                                       */
/*       * N.B.: Se relativo al progressivo riga zero, si rife-  */
/*       *       risce al collo per intero                       */
/*       * N.B.: Se relativo al progressivo riga non-zero, si    */
/*       *       riferisce alla singola riga dell'ordine di spe- */
/*       *       dizione, ed il dato e' stato ripreso dai valori */
/*       *       anagrafici                                      */
/*       * N.B.: Campo attualmente gestito solamente per il      */
/*       *       progressivo riga zero                           */
/*       *-------------------------------------------------------*/
  dim_alt                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Dimensioni del collo : Profondita'                    */
/*       *                                                       */
/*       * N.B.: Se relativo al progressivo riga zero, si rife-  */
/*       *       risce al collo per intero                       */
/*       * N.B.: Se relativo al progressivo riga non-zero, si    */
/*       *       riferisce alla singola riga dell'ordine di spe- */
/*       *       dizione, ed il dato e' stato ripreso dai valori */
/*       *       anagrafici                                      */
/*       * N.B.: Campo attualmente gestito solamente per il      */
/*       *       progressivo riga zero                           */
/*       *-------------------------------------------------------*/
  dim_prf                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Data approntamento collo                              */
/*       *                                                       */
/*       * N.B.: Relativo al progressivo riga zero               */
/*       *-------------------------------------------------------*/
  dat_prc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Annotazione per il collo                              */
/*       *                                                       */
/*       * N.B.: Relativo al progressivo riga zero               */
/*       *-------------------------------------------------------*/
  not_prc                   varchar (80)                not null