/*    *==========================================================*/
/*    * Tabella [iir] Iva Intracomunitaria : righe         {iic} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt,
                                                        num_prg,
                                                        dat_reg),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo entro il movimento                 */
/*       *-------------------------------------------------------*/
  num_prg                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione del movimento                      */
/*       *-------------------------------------------------------*/
  dat_reg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Tipo operazione                                       */
/*       *                                                       */
/*       *  - 0001 : Acquisto                                    */
/*       *  - 0002 : Cessione                                    */
/*       *  - 0011 : Rettifica ad acquisto                       */
/*       *  - 0012 : Rettifica a cessione                        */
/*       *                                                       */
/*       * Riportato da testata movimento                        */
/*       *-------------------------------------------------------*/
  top_iic                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Mese di riferimento del riepilogo da rettificare      */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per rettifiche a cessioni                           */
/*       *-------------------------------------------------------*/
  mes_rif                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Trimestre di riferimento del riepilogo da rettificare */
/*       *                                                       */
/*       * N.B. : Attualmente non trattato in quanto la gestione */
/*       *        attuale riguarda solamente i mensili.          */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per rettifiche a cessioni                           */
/*       *-------------------------------------------------------*/
  tri_rif                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Anno di riferimento del riepilogo da rettificare      */
/*       *                                                       */
/*       * In formato : s.aa                                     */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per rettifiche a cessioni                           */
/*       *-------------------------------------------------------*/
  ann_rif                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare in valuta base delle operazioni             */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per cessioni                                        */
/*       * - Per rettifiche a cessioni                           */
/*       *-------------------------------------------------------*/
  imp_ope                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare in valuta base precedente                   */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per rettifiche a cessioni                           */
/*       *-------------------------------------------------------*/
  imp_pre                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare in valuta delle operazioni, senza decimali  */
/*       *                                                       */
/*       * Comunque deve essere espresso nella valuta dello      */
/*       * Stato membro del fornitore                            */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       *-------------------------------------------------------*/
  iiv_ope                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare in valuta precedente, senza decimali        */
/*       *                                                       */
/*       * Comunque deve essere espresso nella valuta dello      */
/*       * Stato membro del fornitore                            */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per rettifiche ad acquisti                          */
/*       *-------------------------------------------------------*/
  iiv_pre                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice della nomenclatura combinata della merce       */
/*       *                                                       */
/*       * Rappresenta il codice delle merci secondo la nomen-   */
/*       * clatura combinata della tariffa doganale in vigore    */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per cessioni                                        */
/*       * - Per rettifiche a cessioni                           */
/*       *-------------------------------------------------------*/
  nco_ope                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice della nomenclatura combinata precedente        */
/*       *                                                       */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per rettifiche a cessioni                           */
/*       *-------------------------------------------------------*/
  nco_pre                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Massa netta in chilogrammi della merce                */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per cessioni                                        */
/*       *-------------------------------------------------------*/
  mas_net                   bigint  (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' espressa nell'unita' di misura supplementa- */
/*       * re                                                    */
/*       *                                                       */
/*       * Puo' mancare, a seconda della merce trattata          */
/*       *                                                       */
/*       * Rappresenta la quantita' espressa nell'unita' di mi-  */
/*       * sura supplementare per la merce prevista nella tarif- */
/*       * fa doganale in vigore                                 */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per cessioni                                        */
/*       *-------------------------------------------------------*/
  qen_ums                   bigint  (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Valore statistico in valuta base                      */
/*       *                                                       */
/*       * Stabilito partendo dall'ammontare degli acquisti, de- */
/*       * ve comprendere le spese di consegna (trasporto, assi- */
/*       * curazione, ecc.) soltanto fino al luogo di introdu-   */
/*       * zione in Italia, ovvero il valore franco confine ita- */
/*       * liano.                                                */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per cessioni                                        */
/*       * - Per rettifiche a cessioni                           */
/*       *-------------------------------------------------------*/
  vls_ope                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Valore statistico in valuta base precedente           */
/*       *                                                       */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per rettifiche a cessioni                           */
/*       *-------------------------------------------------------*/
  vls_pre                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ISO dello Stato di origine delle merci         */
/*       *                                                       */
/*       * Puo' essere uno stato non appartenente alla CEE       */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       *-------------------------------------------------------*/
  iso_org                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo codice di magazzino                              */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Prodotti di vendita                            */
/*       * - 02 : Semilavorati                                   */
/*       * - 03 : Materie prime                                  */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per cessioni                                        */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       * N.B. : Solo se previsto da personalizzazione          */
/*       *-------------------------------------------------------*/
  tip_mag                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice di magazzino numerico                          */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per cessioni                                        */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       * N.B. : Solo se previsto da personalizzazione          */
/*       *-------------------------------------------------------*/
  num_mag                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta                                                */
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
