/*    *==========================================================*/
/*    * Tabella [zmc] Codici causali di magazzino          {mag} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cau),
/*       *=======================================================*/
/*       * Codice causale per gestione magazzino                 */
/*       *-------------------------------------------------------*/
  cod_cau                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione causale                                   */
/*       *-------------------------------------------------------*/
  des_cau                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione causale in uppercase per ordinamento      */
/*       *-------------------------------------------------------*/
  des_key                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico causale                              */
/*       *-------------------------------------------------------*/
  mne_cau                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Commento interno per la causale                       */
/*       *-------------------------------------------------------*/
  com_cau                   varchar (80)                not null,
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
/*       *-------------------------------------------------------*/
  trt_val                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimentazione quantita' di magazzino            */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Carico                                         */
/*       * - 02 : Scarico                                        */
/*       * - 03 : Movimentazione interna                         */
/*       *                                                       */
/*       * N.B. : Se il tipo trattamento del valore indica un    */
/*       *        costo aggiuntivo o una rettifica di costo,     */
/*       *        questo campo puo' assumere solamente il valo-  */
/*       *        re 00                                          */
/*       *                                                       */
/*       * N.B. : Se il tipo trattamento del valore indica un    */
/*       *        trattamento per impostazione diretta o per     */
/*       *        ripresa del costo standard, questo campo puo'  */
/*       *        assumere solamente il valore 01                */
/*       *-------------------------------------------------------*/
  tip_mdm                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Trattamento conto merce per la causale                */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Puo' riferirsi ad un altro documento il quale  */
/*       *        era un documento di movimentazione di merce in */
/*       *        conto                                          */
/*       * - 02 : Non puo' riferirsi ad un altro documento       */
/*       * - 03 : Il documento stesso e' una movimentazione di   */
/*       *        merce in conto                                 */
/*       *                                                       */
/*       * N.B. : Se il tipo trattamento del valore indica un    */
/*       *        costo aggiuntivo o una rettifica di costo,     */
/*       *        questo campo puo' assumere solamente il valo-  */
/*       *        re 00                                          */
/*       *                                                       */
/*       * N.B. : Se il tipo trattamento del valore indica un    */
/*       *        trattamento per impostazione diretta o per     */
/*       *        ripresa del costo standard, questo campo puo'  */
/*       *        assumere solamente il valore 01 o 02           */
/*       *                                                       */
/*       * N.B. : Se la causale e' di movimentazione interna,    */
/*       *        questo campo puo' assumere solamente il va-    */
/*       *        lore 02                                        */
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
/*       * N.B. : Se la causale non puo' riferirsi ad un altro   */
/*       *        documento questo campo puo' assumere solamen-  */
/*       *        te il valore Spaces.                           */
/*       *        Negli altri casi questo campo puo' assumere    */
/*       *        solamente il valore 'N' o 'T'                  */
/*       *-------------------------------------------------------*/
  tip_mic                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice c/merce di gestione magazzino automaticamente  */
/*       * proposto come default per la causale                  */
/*       *                                                       */
/*       * N.B. : Se la causale non puo' riferirsi ad un altro   */
/*       *        documento questo campo puo' assumere solamen-  */
/*       *        te il valore Spaces.                           */
/*       *-------------------------------------------------------*/
  cod_mic                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No possibilita' di variare il c/merce di gestione  */
/*       * magazzino automaticamente proposto come default per   */
/*       * la causale                                            */
/*       *-------------------------------------------------------*/
  snv_mic                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio automaticamente proposto come default   */
/*       * per la causale                                        */
/*       *                                                       */
/*       * - Spaces : Nessun tipo archivio proposto              */
/*       * - N      : No tipo archivio                           */
/*       * - C      : Cliente                                    */
/*       * - F      : Fornitore                                  */
/*       * - D      : Dipendenza                                 */
/*       * - A      : Agente (Incaricato alla vendita)           */
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
/*       *                                                       */
/*       * - N : No, non e'possibile variare il tipo archivio    */
/*       *           proposto automaticamente come default; in   */
/*       *           questo caso il default deve essere diverso  */
/*       *           da Spaces, e non e' necessario che esso sia */
/*       *           presente nella lista dei tipi archivio am-  */
/*       *           messi per la causale                        */
/*       *-------------------------------------------------------*/
  snv_tar                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Lista dei tipi archivio ammessi per la causale; se    */
/*       * vuota sono ammessi tutti i tipi archivio              */
/*       *-------------------------------------------------------*/
  lst_tar                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Default tipo codice di magazzino per la causale       */
/*       *                                                       */
/*       * Spaces : Nessun default                               */
/*       * P      : Default 'P', prodotto finito                 */
/*       * S      : Default 'S', semilavorato                    */
/*       * M      : Default 'M', materia prima                   */
/*       *-------------------------------------------------------*/
  def_tco                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No possibilita' di variare il default per il tipo  */
/*       * codice di magazzino per la causale                    */
/*       *                                                       */
/*       * N : No                                                */
/*       * S : Si                                                */
/*       *-------------------------------------------------------*/
  snv_tco                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Lista dei tipi codice di magazzino ammessi per la     */
/*       * causale; se vuota sono ammessi tutti i tipi codice    */
/*       *-------------------------------------------------------*/
  lst_tco                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
