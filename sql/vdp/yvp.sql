/*    *==========================================================*/
/*    * Tabella [yvp] Tipi versamenti da produzione        {vdp} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_tmv),
/*       *=======================================================*/
/*       * Codice tipo movimento                                 */
/*       *-------------------------------------------------------*/
  cod_tmv                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo movimento in uppercase               */
/*       *-------------------------------------------------------*/
  des_key                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo movimento                            */
/*       *-------------------------------------------------------*/
  des_tmv                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Password                                              */
/*       *-------------------------------------------------------*/
  pwd_tmv                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice responsabile                                   */
/*       *-------------------------------------------------------*/
  rsp_tmv                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale per il movimento di carico dalla       */
/*       * produzione relativo al versamento da produzione       */
/*       *                                                       */
/*       * N.B. : Se a zero non si esegue questo movimento       */
/*       *        di magazzino                                   */
/*       *-------------------------------------------------------*/
  cau_car                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice c/merce di gestione magazzino per il movimento */
/*       * di carico dalla produzione relativo al versamento da  */
/*       * da produzione                                         */
/*       *                                                       */
/*       * N.B. : Solo se codice causale per movimento di carico */
/*       *        magazzino diverso da zero                      */
/*       *                                                       */
/*       *        Solo causale di magazzino che riguarda         */
/*       *        movimentazione di conto merce                  */
/*       *-------------------------------------------------------*/
  cmc_car                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale per il movimento di scarico consumo    */
/*       * componenti relativo al versamento da produzione       */
/*       *                                                       */
/*       * N.B. : Se a zero non si esegue questo movimento di    */
/*       *        magazzino                                      */
/*       *-------------------------------------------------------*/
  cau_sca                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice c/merce di gestione magazzino per il movimento */
/*       * di scarico componenti relativo al versamento da       */
/*       * produzione                                            */
/*       *                                                       */
/*       * N.B. : Solo se codice causale per movimento di        */
/*       *        scarico magazzino diverso da zero              */
/*       *                                                       */
/*       *        Solo causale di magazzino che riguarda         */
/*       *        movimentazione di conto merce                  */
/*       *-------------------------------------------------------*/
  cmc_sca                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di esplosione della distinta base per lo scarico */
/*       * dei componenti relativo al versamento da produzione   */
/*       *                                                       */
/*       * - 01 : Ad un livello                                  */
/*       * - 02 : Scalare totale                                 */
/*       * - 03 : Scalare fino ai semilavorati finali            */
/*       *                                                       */
/*       * N.B. : Solo se codice causale per movimento di        */
/*       *        scarico magazzino diverso da zero              */
/*       *-------------------------------------------------------*/
  tde_dib                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Possibilita' di impostazione manuale di ulteriori     */
/*       * consumi a fronte della produzione                     */
/*       *                                                       */
/*       * - S : Si'                                             */
/*       * - N : No                                              */
/*       *-------------------------------------------------------*/
  snx_cad                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale per il movimento di scarico consumo    */
/*       * componenti addizionali da impostazione manuale        */
/*       *                                                       */
/*       * N.B. : Se a zero non si accetteranno impostazioni     */
/*       *        manuali di consumi addizionali                 */
/*       *-------------------------------------------------------*/
  cau_cad                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice c/merce di gestione magazzino per il movimento */
/*       * di scarico consumo componenti addizionali da          */
/*       * impostazione manuale                                  */
/*       *                                                       */
/*       * N.B. : Solo se codice causale per movimento di        */
/*       *        scarico consumo componenti addizionali         */
/*       *        diverso da zero                                */
/*       *                                                       */
/*       *        Solo causale di magazzino che riguarda         */
/*       *        movimentazione di conto merce                  */
/*       *-------------------------------------------------------*/
  cmc_cad                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo origine del documento associato al tipo movimen- */
/*       * to                                                    */
/*       *                                                       */
/*       *   - 01 : Manuale                                      */
/*       *   - 11 : Automatica                                   */
/*       *-------------------------------------------------------*/
  org_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo provenienza del documento se origine automatica  */
/*       *                                                       */
/*       * - Se tipo origine del documento 11                    */
/*       *   - 01 : Da evasione automatica commesse produzione   */
/*       *-------------------------------------------------------*/
  prv_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla numerazione per il documento associato al tipo  */
/*       * movimento                                             */
/*       *-------------------------------------------------------*/
  sgl_num                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Movimento a fronte di :                               */
/*       *                                                       */
/*       * - 01 : Niente                                         */
/*       * - 02 : Commessa di produzione                         */
/*       *-------------------------------------------------------*/
  mov_afd                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Movimento che contribuisce a chiudere la commessa di  */
/*       * produzione; solo se 'Movimento a fronte di' diverso   */
/*       * da 01                                                 */
/*       *                                                       */
/*       * - Spaces : Non significativo                          */
/*       * - S      : Si'                                        */
/*       * - N      : No                                         */
/*       *-------------------------------------------------------*/
  chi_com                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Obbligatorieta' del riferimento per il movimento a    */
/*       * fronte; solo se 'Movimento a fronte di' diverso da 01 */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Obbligatorio                                   */
/*       * - 02 : Facoltativo                                    */
/*       *-------------------------------------------------------*/
  obl_afd                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento di default se movimento a fronte di    */
/*       * qualcosa                                              */
/*       *                                                       */
/*       * N.B.: Significativo solo se se 'Movimento a fronte di */
/*       *       diverso da 01                                   */
/*       *-------------------------------------------------------*/
  def_tmf                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Modificabilita' del tipo movimento di default per il  */
/*       * movimento a fronte                                    */
/*       *                                                       */
/*       * - Spaces : Non significativo                          */
/*       * - S      : Si'                                        */
/*       * - N      : No                                         */
/*       *                                                       */
/*       * N.B.: Significativo solo se se 'Movimento a fronte di */
/*       *       diverso da 01                                   */
/*       *       Significativo solo se se 'Tipo movimento di     */
/*       *       default' diverso da Spaces                      */
/*       *-------------------------------------------------------*/
  mod_tmf                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione di origine per il documento       *
/*       *-------------------------------------------------------*/
  cod_dsl                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Default modulo da stampare                            */
/*       *-------------------------------------------------------*/
  def_mdf                   varchar (08)                not null







