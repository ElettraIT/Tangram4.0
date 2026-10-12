/*    *==========================================================*/
/*    * Tabella [ybf] Tabella tipi movimento               {bfo} */
/*    *                                                          */
/*    * Aggiornamento 30.05.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_tmo),
/*       *=======================================================*/
/*       * Codice tipo movimento                                 */
/*       *-------------------------------------------------------*/
  cod_tmo                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per uso interno uppercase                 */
/*       *-------------------------------------------------------*/
  des_key                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per uso interno                           */
/*       *-------------------------------------------------------*/
  des_tmo                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Password per il tipo movimento                        */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  pwd_tmo                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No interessa la fatturazione                       */
/*       *                                                       */
/*       * - 01 : No                                             */
/*       * - 02 : Si, il documento sara' in seguito sottoposto a */
/*       *            fatturazione                               */
/*       * - 03 : Si, il documento stesso e' anche una fattura   */
/*       *                                                       */
/*       * N.B. : Il valore 03 non e' attualmente ammesso in     */
/*       *        quanto non sono gestite le fatture             */
/*       *        accompagnatorie per i fornitori                */
/*       *-------------------------------------------------------*/
  int_ftr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento di fatturazione                        */
/*       *                                                       */
/*       * - Se movimento che interessa la fatturazione 01       */
/*       *      - Valore fisso Spaces, non significativo         */
/*       *                                                       */
/*       * - Se movimento che interessa la fatturazione 02       */
/*       *      - Valore fisso Spaces, non significativo         */
/*       *                                                       */
/*       * - Se movimento che interessa la fatturazione 03       */
/*       *      - Codice tipo movimento di fatturazione          */
/*       *        corrispondente alla componente 'fattura' del   */
/*       *        presente tipo movimento di bollettazione       */
/*       *                                                       */
/*       * N.B. : Attualmente il valore di questo campo sara'    */
/*       *        sempre a spaces, in quanto non sono gestite le */
/*       *        fatture accompagnatorie per i fornitori        */
/*       *-------------------------------------------------------*/
  tmo_ftr                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale per movimento magazzino                */
/*       *                                                       */
/*       * N.B. : Se a zero non si esegue movimentazione di      */
/*       *        magazzino a fronte di questo documento         */
/*       *-------------------------------------------------------*/
  cau_mag                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo trattamento per le statistiche                   */
/*       *                                                       */
/*       * - A  : Acquisto                                       */
/*       * - V  : Vendita                                        */
/*       * - RV : Reso vendita                                   */
/*       * - RA : Reso acquisto                                  */
/*       * - N  : Neutro                                         */
/*       *                                                       */
/*       * N.B. : Campo proveniente da tipo movimento            */
/*       *-------------------------------------------------------*/
  tip_trs                   varchar (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice c/merce di gestione magazzino                  */
/*       *                                                       */
/*       * N.B. : Solo se codice causale movimento magazzino     */
/*       *        diverso da zero                                */
/*       *                                                       */
/*       *        Solo causale di magazzino che riguarda movi-   */
/*       *        mentazione di conto merce                      */
/*       *-------------------------------------------------------*/
  cod_mic                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale di magazzino aggiuntiva                */
/*       *-------------------------------------------------------*/
  cam_agg                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio automaticamente proposto come default   */
/*       * per il tipo movimento                                 */
/*       *                                                       */
/*       * - Spaces : Nessun default                             */
/*       * - N      : No tipo archivio                           */
/*       * - C      : Cliente                                    */
/*       * - F      : Fornitore                                  */
/*       * - D      : Dipendenza                                 */
/*       *                                                       */
/*       * N.B. : Se il documento interessa la fatturazione il   */
/*       *        tipo archivio deve essere pari a 'C'           */
/*       *-------------------------------------------------------*/
  def_tar                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No possibilita' di variare il tipo archivio        */
/*       * proposto come default                                 */
/*       *                                                       */
/*       * - S : Si, e' possibile variare il tipo archivio pro-  */
/*       *           posto automaticamente come default;         */
/*       *           in questo caso il default puo' anche essere */
/*       *           a Spaces, pero' e' necessario che esista un */
/*       *           elemento nella lista dei tipi archivio      */
/*       *           ammessi per il tipo movimento corrisponden- */
/*       *           te al tipo archivio impostato               */
/*       *                                                       */
/*       * - N : No, non e' possibile variare il tipo archivio   */
/*       *           proposto automaticamente come default;      */
/*       *           in questo caso il default deve essere       */
/*       *           diverso da Spaces e non e' necessario che   */
/*       *           esso sia presente nella lista dei tipi      */
/*       *           archivio ammessi per il tipo movimento      */
/*       *                                                       */
/*       * N.B. : Se il documento interessa la fatturazione il   */
/*       *        valore impostato deve essere pari a 'N'        */
/*       *-------------------------------------------------------*/
  snv_tar                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Lista tipi archivio ammessi: se vuota sono ammessi    */
/*       *                              tutti i tipi archivio    */
/*       *-------------------------------------------------------*/
  lst_tar                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo origine del documento associato a tipo movimento */
/*       *                                                       */
/*       * - 01 : Manuale                                        */
/*       * - 11 : Automatica                                     */
/*       *-------------------------------------------------------*/
  org_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo provenienza del documento se origine automatica  */
/*       *                                                       */
/*       * - Se tipo origine del documento 11                    */
/*       *   - 01 : Da evasione ordini                           */
/*       *-------------------------------------------------------*/
  prv_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Movimento a fronte di :                               */
/*       *                                                       */
/*       * - 01 : Niente                                         */
/*       * - 02 : Ordine fornitore                               */
/*       * - 03 : Ordine di ricevimento merce                    */
/*       * - 04 : Bolla di consegna                              */
/*       *-------------------------------------------------------*/
  mov_afd                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento di default se movimento a fronte       */
/*       *                                                       */
/*       * N.B.: Significativo solo se campo precedente diverso  */
/*       *       da zero                                         */
/*       *-------------------------------------------------------*/
  def_tmf                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Validita' per le dipendenze                           */
/*       *                                                       */
/*       *  - spaces : per tutte le dipendenze                   */
/*       *  - S      : solo per la Sede                          */
/*       *  - D      : solo per le dipendenze                    */
/*       *  - X      : solo per le dipendenze                    */
/*       *             indicate                                  */
/*       *-------------------------------------------------------*/
  vld_dpz                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Default accettazione tipo riga corpo                  */
/*       *-------------------------------------------------------*/
  def_tpr                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione                                   */
/*       *-------------------------------------------------------*/
  cod_dsl                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Status per il tipo movimento                          */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  sta_tus                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di determinazione dello status                   */
/*       *                                                       */
/*       * Solo se status diverso da 01                          */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  sta_tud                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice tipo movimento di riferimento                  */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  sta_tuc                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di trattamento nelle statistiche per i      */
/*       * documenti precedenti il cambiamento di status         */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  sta_tux                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
