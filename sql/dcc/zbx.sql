/*    *==========================================================*/
/*    * Tabella [zbx] Scaglioni spese bollo [zbo]          {dcc} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_spb,
                                                        tip_pag,
                                                        num_prg),
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
/*       * Progressivo scaglione 1..10                           */
/*       *-------------------------------------------------------*/
  num_prg                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Scaglione                                             */
/*       *-------------------------------------------------------*/
  tbe_scg                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare sullo scaglione                             */
/*       *                                                       */
/*       * Solo se tipo di automatismo 02                        */
/*       *-------------------------------------------------------*/
  tbe_asc                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Percentuale sullo scaglione                           */
/*       *                                                       */
/*       * Solo se tipo di automatismo 03                        */
/*       *-------------------------------------------------------*/
  tbe_psc                   decimal (05,02)             not null