/*    *==========================================================*/
/*    * Tabella [rsd] Riscossioni scadenze                 {gep} */
/*    *                                                          */
/*    * Aggiornamento 22.10.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_rsd),
/*       *=======================================================*/
/*       * Numero riscossione : s.aa.nnnnnnnn                    */
/*       *-------------------------------------------------------*/
  num_rsd                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di riscossione           */
/*       *-------------------------------------------------------*/
  dtr_rsd                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di riscossione                          */
/*       *-------------------------------------------------------*/
  drc_rsd                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di riscossione                          */
/*       *-------------------------------------------------------*/
  npc_rsd                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di riscossione                              */
/*       *                                                       */
/*       * - 01 : Riscossione per Contanti                       */
/*       * - 02 : Riscossione con Assegno                        */
/*       * - 03 : Riscossione per Bonifico bancario              */
/*       * - 04 : Riscossione per C/C postale                    */
/*       * - 21 : Pagamento per Contanti                         */
/*       * - 22 : Pagamento con Assegno                          */
/*       * - 23 : Pagamento per Bonifico bancario                */
/*       * - 24 : Pagamento per C/C postale                      */
/*       * - 50 : Compensazione scadenze                         */
/*       *-------------------------------------------------------*/
  mod_rsd                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di riscossione                                   */
/*       *                                                       */
/*       * - 01 : A fronte scadenze, normale                     */
/*       * - 02 : Anticipo su scadenze future                    */
/*       *                                                       */
/*       * N.B. : Solo se modalita' di riscossione :             */
/*       *        - 01 : Riscossione per Contanti                */
/*       *        - 02 : Riscossione con Assegno                 */
/*       *        - 03 : Riscossione per Bonifico bancario       */
/*       *        - 04 : Riscossione per C/C postale             */
/*       *-------------------------------------------------------*/
  tip_rsd                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo valuta per la riscossione                        */
/*       *                                                       */
/*       * - 01 : Riscossione in valuta base                     */
/*       * - 02 : Riscossione in valuta diversa da quella base   */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito, e forzato al    */
/*       *        valore 01                                      */
/*       *-------------------------------------------------------*/
  tvl_rsd                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo debitore                                         */
/*       *                                                       */
/*       * - 01 : Cliente                                        */
/*       *-------------------------------------------------------*/
  tip_dbt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice debitore                                       */
/*       *-------------------------------------------------------*/
  cod_dbt                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza del debitore                        */
/*       *-------------------------------------------------------*/
  dpz_dbt                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra cassa per la riscossione se per contan- */
/*       * ti o per assegno, oppure codice nostra banca per la   */
/*       * riscossione se tramite bonifico bancario, oppure co-  */
/*       * dice nostro conto corrente postale se tramite conto   */
/*       * corrente postale                                      */
/*       *                                                       */
/*       * Codice nostra cassa per il pagamento se per contanti  */
/*       * oppure codice nostra banca per la il pagamento se per */
/*       * assegno o per bonifico bancario, oppure codice nostro */
/*       * conto corrente postale se tramite c.c.p.              */
/*       *-------------------------------------------------------*/
  cbp_rsd                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta per la riscossione                             */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *-------------------------------------------------------*/
  sgl_vpf                   varchar (03)                not null,
  dec_vpf                   tinyint (01)                not null,
  tdc_vpf                   varchar (01)                not null,
  cdc_vpf                   decimal (12,05)             not null,
/*       *-------------------------------------------------------*/
/*       * Importo riscossione, in valuta                        */
/*       *-------------------------------------------------------*/
  iiv_rsd                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo riscossione                                   */
/*       *                                                       */
/*       * N.B. : Relativo sia a scadenze in valuta base, che    */
/*       *        non.                                           */
/*       *-------------------------------------------------------*/
  imp_rsd                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo spese addebitate dalla banca per la riscos-   */
/*       * sione in caso di modalita' di riscossione pari a :    */
/*       *                                                       */
/*       * - 03 : Bonifico bancario                              */
/*       *                                                       */
/*       * oppure                                                */
/*       *                                                       */
/*       * Importo spese addebitate dalla posta per la riscos-   */
/*       * sione in caso di modalita' di riscossione pari a :    */
/*       *                                                       */
/*       * - 04 : Bonifico postale                               */
/*       *-------------------------------------------------------*/
  spe_rsd                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento per la riscossione                     */
/*       *-------------------------------------------------------*/
  ddo_rsd                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento per la riscossione                   */
/*       *-------------------------------------------------------*/
  ndo_rsd                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero scadenza generata a fronte della riscossione   */
/*       *                                                       */
/*       * N.B. : Solo se tipo riscossione :                     */
/*       *                                                       */
/*       *        - 02 : Anticipo su scadenze future             */
/*       *-------------------------------------------------------*/
  nsc_rsd                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero ulteriore operazione post-riscossione, nel     */
/*       * formato : s.aa.nnnnnnnn                               */
/*       *-------------------------------------------------------*/
  num_uor                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di pulizia effettuabile                         */
/*       *-------------------------------------------------------*/
  flg_pul                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
