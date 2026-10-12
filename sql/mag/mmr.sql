/*    *==========================================================*/
/*    * Tabella [mmr] Movimenti di magazzino : righe             */
/*    *                                                          */
/*    * N.B.: eliminata la data registrazione dalla chiave       */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt,
                                                        num_prg),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo entro il movimento                 */
/*       *-------------------------------------------------------*/
  num_prg                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di registrazione movimento                       */
/*       *-------------------------------------------------------*/
  dat_reg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza                                     */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale per gestione magazzino                 */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_cau                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di trattamento per il valore per la causale      */
/*       *                                                       */
/*       *   - N : Nessun trattamento del valore                 */
/*       *   - I : Trattamento del valore per impostazione       */
/*       *   - S : Trattamento del valore, ma senza impostazione */
/*       *         in quanto valorizzazione al costo standard    */
/*       *   - C : Trattamento del solo valore, senza quantita'  */
/*       *         come costo aggiuntivo                         */
/*       *   - X : Trattamento del solo valore, senza quantita'  */
/*       *         come costo aggiuntivo riferito alla fine e-   */
/*       *         sercizio precedente                           */
/*       *   - R : Trattamento del solo valore, senza quantita'  */
/*       *         come rettifica ad un costo                    */
/*       *   - Y : Trattamento del solo valore, senza quantita'  */
/*       *         come rettifica ad un costo riferito alla fi-  */
/*       *         ne esercizio precedente                       */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  trt_val                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimentazione di magazzino                      */
/*       *                                                       */
/*       * - 01 : Carico                                         */
/*       * - 02 : Scarico                                        */
/*       * - 03 : Movimentazione interna                         */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  tip_mdm                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Trattamento conto merce per la causale                */
/*       *                                                       */
/*       * - 01 : Puo' riferirsi ad un altro documento il quale  */
/*       *        era un documento di movimentazione di merce in */
/*       *        conto                                          */
/*       * - 02 : Non puo' riferirsi ad un altro documento       */
/*       * - 03 : Il documento stesso e' una movimentazione di   */
/*       *        merce in conto                                 */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  trt_mic                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo conto merce per la causale                       */
/*       *                                                       */
/*       * - Spaces : Non significativo                          */
/*       * - N      : Merce presso terzi ma non di loro pro-     */
/*       *            prieta'                                    */
/*       * - T      : Merce presso di noi ma non di nostra pro-  */
/*       *            prieta'                                    */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  tip_mic                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice c/merce di gestione magazzino automaticamente  */
/*       * proposto come default per la causale                  */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_ctm                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento                                        */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null,  
/*       *-------------------------------------------------------*/
/*       * Numero documento                                      */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  num_doc                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione                                   */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_dsl                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione di destinazione                   */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_dsd                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio effettivo                               */
/*       *                                                       */
/*       * - Spaces : No tipo archivio                           */
/*       * - N      : No tipo archivio                           */
/*       * - C      : Cliente                                    */
/*       * - F      : Fornitore                                  */
/*       * - D      : Nostra dipendenza                          */
/*       * - A      : Agente (Incaricato alla vendita)           */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza relativo al codice archivio         */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  dpz_arc                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo codice di magazzino                              */
/*       *                                                       */
/*       * - 01 : Prodotti di vendita                            */
/*       * - 02 : Semilavorati                                   */
/*       * - 03 : Materie prime                                  */
/*       * - 04 : Materiali vari                                 */
/*       *-------------------------------------------------------*/
  tip_mag                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto numerico                              */
/*       *-------------------------------------------------------*/
  num_mag                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice alfanumerico magazzino                         *
/*       *-------------------------------------------------------*/
  alf_mag                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla della variante di magazzino                     */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  var_mag                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura da anagrafica codice magazzino       */
/*       *-------------------------------------------------------*/
  udm_ana                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura per il movimento                     */
/*       *-------------------------------------------------------*/
  udm_mov                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero partita di magazzino : s.aa.dd.nnnnnn          */
/*       *                                                       */
/*       * N.B. : Questo campo attualmente non e' gestito        */
/*       *-------------------------------------------------------*/
  num_pmg                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento di riferimento                         */
/*       *                                                       */
/*       * Questo campo attualmente e' gestito solo se presente  */
/*       * la personalizzazione 'pgm/mag/mov/mag300[trp-ttm]'    */
/*       *-------------------------------------------------------*/
  ddo_rif                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento di riferimento                       */
/*       *                                                       */
/*       * Questo campo attualmente e' gestito solo se presente  */
/*       * la personalizzazione 'pgm/mag/mov/mag300[trp-ttm]'    */
/*       *-------------------------------------------------------*/
  ndo_rif                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' movimento                                   */
/*       *                                                       */
/*       * N.B. : Se il tipo trattamento del valore indica un    */
/*       *        costo aggiuntivo o una rettifica di costo,     */
/*       *        questo campo viene forzato a zero              */
/*       *-------------------------------------------------------*/
  qta_mov                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Costo unitario diretto, ovvero da impostazione        */
/*       *                                                       */
/*       * N.B. : Se il tipo trattamento del valore non indica   */
/*       *        un trattamento del valore da impostazione di-  */
/*       *        retta, questo campo viene forzato a zero, a    */
/*       *        meno che non sia presente la personalizzazione */
/*       *        'pgm/mag/mov/mag300[trp-ttm]'                  */
/*       *-------------------------------------------------------*/
  cun_mov                   bigint  (11)                not null,
  val_mov                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags bloccanti                                       */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       * 04 : (libero)                                         */
/*       * 05 : (libero)                                         */
/*       * 06 : (libero)                                         */
/*       * 07 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_bl1                   varchar (01)                not null,
  flg_bl2                   varchar (01)                not null,
  flg_bl3                   varchar (01)                not null,
  flg_bl4                   varchar (01)                not null,
  flg_bl5                   varchar (01)                not null,
  flg_bl6                   varchar (01)                not null,
  flg_bl7                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags non bloccanti                                   */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_nb1                   varchar (01)                not null,
  flg_nb2                   varchar (01)                not null,
  flg_nb3                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di pulizia effettuabile                         */
/*       *-------------------------------------------------------*/
  flg_pul                   varchar (01)                not null
