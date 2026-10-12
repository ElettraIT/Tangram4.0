/*    *==========================================================*/
/*    * Tabella [aaq] Dati generali di acquisto prodotti   {dcf} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_mag,
                                                        num_mag),
/*       *=======================================================*/
/*       * Tipo codice di magazzino                              */
/*       *                                                       */
/*       * - 01 : Prodotti di vendita                            */
/*       * - 03 : Materie prime                                  */
/*       * - 04 : Materiali vari                                 */
/*       *-------------------------------------------------------*/
  tip_mag                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto numerico                              */
/*       *-------------------------------------------------------*/
  num_mag                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice Iva per l'acquisto, solo se diverso da quello  */
/*       * per la vendita                                        */
/*       *-------------------------------------------------------*/
  cid_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Conto di contabilita' generale usato come contro-     */
/*       * partita per gli acquisti del prodotto                 */
/*       *-------------------------------------------------------*/
  ctp_acq                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice del fornitore preferenziale, opzionale         */
/*       *-------------------------------------------------------*/
  dcf_pfz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza fornitore preferenziale, opzionale  */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dpz_pfz                   varchar (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla valuta per il prezzo d'acquisto di riferimento  */
/*       *-------------------------------------------------------*/
  sgl_vlt                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Decimali valuta per il prezzo di riferimento          */
/*       *-------------------------------------------------------*/
  dec_vlt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Decimali per il prezzo di riferimento                 */
/*       *-------------------------------------------------------*/
  dec_prz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Prezzo d'acquisto di riferimento                      */
/*       *-------------------------------------------------------*/
  prz_acq                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data aggiornamento prezzo d'acquisto di riferimento   */
/*       *-------------------------------------------------------*/
  dat_agg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice del produttore reale del prodotto, opzionale   */
/*       *-------------------------------------------------------*/
  cod_pdt                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice del prodotto secondo il produttore reale del   */
/*       * prodotto, opzionale                                   */
/*       *-------------------------------------------------------*/
  cdp_pdt                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di espressione tempo di approvigionamento   */
/*       *-------------------------------------------------------*/
  mde_tda                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tempo di approvigionamento medio del prodotto,        */
/*       * espresso nella modalita' stabilita                    */
/*       *-------------------------------------------------------*/
  tmp_apv                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Lotto di acquisto di riferimento                      */
/*       *-------------------------------------------------------*/
  lot_acq                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di esposizione della riga di ordine a fornitore  */
/*       * nitore                                                */
/*       *                                                       */
/*       * - 1 : Quantita', prezzo, sconti, importo              */
/*       * - 2 : Solo quantita' e importo                        */
/*       * - 3 : Solo importo                                    */
/*       * - 4 : Solo importo se quantita pari a 1,000, al-      */
/*       *       trimenti tutto come il tipo 1                   */
/*       * - 5 : Solo importo se quantita pari a 1,000, al-      */
/*       *       trimenti tutto come il tipo 2                   */
/*       *                                                       */
/*       * N.B. : Campo attualmente forzato al valore = 1        */
/*       *-------------------------------------------------------*/
  epz_rgf                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della seconda quantita' nella     */
/*       * fatturazione                                          */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_2qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la seconda quantita'              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_2qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della terza quantita' nella       */
/*       * fatturazione                                          */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_3qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la terza quantita'                */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_3qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione del secondo prezzo nella fattura- */
/*       * zione                                                 */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_2pz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Decimali del secondo prezzo nella fatturazione        */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_2pz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Indice per determinazione automatica prezzo listino   */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  aut_lst                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo variante per l'acquisto                          */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  tip_vac                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null

