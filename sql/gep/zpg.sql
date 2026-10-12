/*    *==========================================================*/
/*    * Tabella [zpg] Codici di pagamento                  {gep} */
/*    *                                                          */
/*    * Aggiornamento 29.03.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_pag),
/*       *=======================================================*/
/*       * Codice pagamento                                      */
/*       *-------------------------------------------------------*/
  cod_pag                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico                                      */
/*       *-------------------------------------------------------*/
  cod_mne                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione codice di pagamento                       */
/*       *-------------------------------------------------------*/
  des_pag                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di pagamento                                     */
/*       *                                                       */
/*       * - 01 : Rimessa diretta                                */
/*       * - 02 : Incasso elettronico                            */
/*       * - 03 : Ri.Ba.                                         */
/*       * - 04 : C.d.O.                                         */
/*       * - 05 : Mav - Mediante avviso                          */
/*       * - 06 : Rid - Accredito preautorizzato in conto        */
/*       * - 07 : Bonifico bancario                              */
/*       * - 08 : Conto corrente postale                         */
/*       * - 09 : Ricevuta bancaria tradizionale                 */
/*       * - 10 : Tratta                                         */
/*       * - 11 : Paghero'                                       */
/*       *-------------------------------------------------------*/
  tip_pag                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di scadenze : 01..96                           */
/*       *-------------------------------------------------------*/
  num_sca                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Decorrenza della prima scadenza                       */
/*       *                                                       */
/*       * - 01 : Manuale                                        */
/*       * - 02 : Ad una data prefissata (gg/mm)                 */
/*       * - 03 : A vista                                        */
/*       * - 04 : A  15 giorni                                   */
/*       * - 05 : A  30 giorni                                   */
/*       * - 06 : A  45 giorni                                   */
/*       * - 07 : A  60 giorni                                   */
/*       * - 08 : A  90 giorni                                   */
/*       * - 09 : A 120 giorni                                   */
/*       * - 10 : A 150 giorni                                   */
/*       * - 11 : A 180 giorni                                   */
/*       * - 12 : A 'n' giorni                                   */
/*       *                                                       */
/*       * N.B. : I valori 01, 02, 03, sono ammessi solo se il   */
/*       *        numero scadenze e' pari a 1                    */
/*       *-------------------------------------------------------*/
  dec_prs                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data prefissata, mese, solo se decorrenza prima sca-  */
/*       * denza pari a 02                                       */
/*       *                                                       */
/*       * N.B. : Se decorrenza prima scadenza a 'n' giorni, il  */
/*       *        mese ed il giorno prefissato contengono il nr. */
/*       *        di giorni (1..999)                             */
/*       *-------------------------------------------------------*/
  dap_mes                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data prefissata, giorno, solo se decorrenza prima     */
/*       * scadenza pari a 02                                    */
/*       *                                                       */
/*       * N.B. : Se decorrenza prima scadenza a 'n' giorni, il  */
/*       *        mese ed il giorno prefissato contengono il nr. */
/*       *        di giorni (1..999)                             */
/*       *-------------------------------------------------------*/
  dap_gio                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Giorni di intervallo tra due scadenze                 */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Di 30 giorni in 30 giorni                      */
/*       * - 02 : Di 15 giorni in 15 giorni                      */
/*       * - 11 : Di 'nn' giorni                                 */
/*       *-------------------------------------------------------*/
  ggg_int                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di scadenza nel mese                             */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Giorno di decorrenza                           */
/*       * - 02 : Fine mese                                      */
/*       * - 03 : Giorno fisso nel mese                          */
/*       * - 04 : Giorno fisso nel mese successivo a fine mese   */
/*       *                                                       */
/*       * N.B. : Non significativo se la decorrenza per la pri- */
/*       *        ma scadenza ha il valore 01, 02, 03.           */
/*       *-------------------------------------------------------*/
  tip_scm                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Giorno di scadenza nel mese                           */
/*       *                                                       */
/*       * - 00     : non significativo                          */
/*       * - 01..28 : solo se tipo scadenza nel mese 03          */
/*       *-------------------------------------------------------*/
  gio_scm                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di pagamento - tabella per Sdi              */
/*       *                                                       */
/*       * 'MP01' : Contanti                                     */
/*       * 'MP02' : Assegno                                      */
/*       * 'MP03' : Assegno Circolare                            */
/*       * 'MP04' : Contanti presso Tesoreria                    */
/*       * 'MP05' : Bonifico                                     */
/*       * 'MP06' : Vaglia cambiario                             */
/*       * 'MP07' : Bollettino bancario                          */
/*       * 'MP08' : carta di pagamento                           */
/*       * 'MP09' : RID		                              */		
/*       * 'MP10' : RID utenze	                              */
/*       * 'MP11' : RID veloce			                */
/*       * 'MP12' : RIBA				                */
/*       * 'MP13' : MAV				                */
/*       * 'MP14' : quietanza erario			         */
/*       * 'MP15' : giroconto su conti di contabilita' sp.       */
/*       * 'MP16' : domiciliazione bancaria		         */
/*       * 'MP17' : domiciliazione postale		         */
/*       * 'MP18' : bollettino di c/c postale		         */
/*       * 'MP19' : SEPA Direct Debit			         */
/*       * 'MP20' : SEPA Direct Debit CORE		         */
/*       * 'MP21' : SEPA Direct Debit B2B			         */
/*       * 'MP22' : Trattenuta su somme gia' riscosse	         */
/*       *-------------------------------------------------------*/
  mdp_sdi                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null

