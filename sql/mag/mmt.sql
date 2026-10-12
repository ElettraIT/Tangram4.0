/*    *==========================================================*/
/*    * Tabella [mmt] Movimenti di magazzino : testate           */
/*    *                                                          */
/*    * N.B.: eliminata la data registrazione dalla chiave       */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di registrazione movimento                       */
/*       *-------------------------------------------------------*/
  dat_reg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza                                     */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale per gestione magazzino                 */
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
/*       * N.B. Riportato dalla causale di magazzino             */
/*       *-------------------------------------------------------*/
  trt_val                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimentazione di magazzino                      */
/*       *                                                       */
/*       * - 01 : Carico                                         */
/*       * - 02 : Scarico                                        */
/*       * - 03 : Movimentazione interna                         */
/*       *                                                       */
/*       * N.B. Riportato dalla causale di magazzino             */
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
/*       * N.B. Riportato dalla causale di magazzino             */
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
/*       * N.B. Riportato dalla causale di magazzino             */
/*       *-------------------------------------------------------*/
  tip_mic                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice c/merce di gestione magazzino automaticamente  */
/*       * proposto come default per la causale                  */
/*       *                                                       */
/*       * N.B. Riportato dalla causale di magazzino             */
/*       *-------------------------------------------------------*/
  cod_ctm                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No possibilita' di variare il c/merce di gestione  */
/*       * magazzino automaticamente proposto come default per   */
/*       * la causale                                            */
/*       *                                                       */
/*       * N.B. Riportato dalla causale di magazzino             */
/*       *-------------------------------------------------------*/
  snv_ctm                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio automaticamente proposto come default   */
/*       * per la causale                                        */
/*       *                                                       */
/*       * - Spaces : Nessun tipo archivio proposto              */
/*       * - N      : No tipo archivio                           */
/*       * - C      : Cliente                                    */
/*       * - F      : Fornitore                                  */
/*       * - D      : Dipendenza                                 */
/*       *                                                       */
/*       * N.B. Riportato dalla causale di magazzino             */
/*       *-------------------------------------------------------*/
  def_tar                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No possibilita' di variare il tipo archivio propo- */
/*       * sto come default per la causale                       */
/*       *                                                       */
/*       * - S : Si, e'possibile variare il tipo archivio propo- */
/*       *           sto automaticamente come default; in questo */
/*       *           caso il default puo' anche essere a Spaces, */
/*       *           pero' e' necessario che esista un elemento  */
/*       *           nella lista dei tipi archivio ammessi per   */
/*       *           la causale corrispondente al tipo archivio  */
/*       *           impostato                                   */
/*       * - N : No, non e'possibile variare il tipo archivio    */
/*       *           proposto automaticamente come default; in   */
/*       *           questo caso il default deve essere diverso  */
/*       *           da Spaces, e non e' necessario che esso sia */
/*       *           presente nella lista dei tipi archivio am-  */
/*       *           messi per la causale                        */
/*       *                                                       */
/*       * N.B. Riportato dalla causale di magazzino             */
/*       *-------------------------------------------------------*/
  snv_tar                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Lista dei tipi archivio ammessi per la causale; se    */
/*       * vuota sono ammessi tutti i tipi archivio              */
/*       *                                                       */
/*       * N.B. Riportato dalla causale di magazzino             */
/*       *-------------------------------------------------------*/
  lst_tar                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione automaticamente proposto come     */
/*       * default per la causale                                */
/*       *                                                       */
/*       * N.B. Riportato dalla causale di magazzino             */
/*       *-------------------------------------------------------*/
  def_dsl                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione di destinazione automaticamente   */
/*       * proposto come default per la causale                  */
/*       *                                                       */
/*       * N.B. Riportato dalla causale di magazzino             */
/*       *-------------------------------------------------------*/
  def_dsd                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No possibilita' di variare il codice dislocazione  */
/*       * automaticamente proposto come default per la causale  */
/*       *                                                       */
/*       * N.B. Riportato dalla causale di magazzino             */
/*       *-------------------------------------------------------*/
  snv_dsl                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto merce effettivo                          */
/*       *-------------------------------------------------------*/
  eff_ctm                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento                                        */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null,  
/*       *-------------------------------------------------------*/
/*       * Numero documento                                      */
/*       *-------------------------------------------------------*/
  num_doc                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione                                   */
/*       *-------------------------------------------------------*/
  cod_dsl                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione di destinazione                   */
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
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza relativo al codice archivio         */
/*       *-------------------------------------------------------*/
  dpz_arc                   varchar (07)                not null,
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
  flg_pul                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null

