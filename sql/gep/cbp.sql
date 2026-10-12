/*    *==========================================================*/
/*    * Tabella [cbp] Banche                               {gep} */
/*    *                                                          */
/*    * Aggiornamento 03.08.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cbp),
/*       *=======================================================*/
/*       * Codice alfanumerico nostra banca                      */
/*       *-------------------------------------------------------*/
  cod_cbp                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificatore numerico nostra banca                  */
/*       *-------------------------------------------------------*/
  ide_cbp                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione nostra banca                              */
/*       *-------------------------------------------------------*/
  des_cbp                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ABI istituto di credito                        */
/*       *-------------------------------------------------------*/
  cod_abi                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CAB istituto di credito                        */
/*       *-------------------------------------------------------*/
  cod_cab                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla del conto corrente bancario                     */
/*       *-------------------------------------------------------*/
  sgl_ccb                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per il conto corrente            */
/*       *-------------------------------------------------------*/
  stc_cco                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per bonifici bancari attivi      */
/*       *-------------------------------------------------------*/
  stc_bba                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per bonifici bancari passivi     */
/*       *-------------------------------------------------------*/
  stc_bbp                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per presentazione distinte al    */
/*       * S.B.F. a maturazione di valuta                        */
/*       *-------------------------------------------------------*/
  stc_psm                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per presentazione distinte al    */
/*       * S.B.F. ad accredito immediato                         */
/*       *-------------------------------------------------------*/
  stc_psi                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per presentazione distinte al    */
/*       * dopo incasso                                          */
/*       *-------------------------------------------------------*/
  stc_pdi                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per presentazione distinte al    */
/*       * allo sconto                                           */
/*       *-------------------------------------------------------*/
  stc_psc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per anticipazioni in valuta      */
/*       *-------------------------------------------------------*/
  stc_anv                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per differenze cambio passive    */
/*       *-------------------------------------------------------*/
  stc_dfp                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per differenze cambio attive     */
/*       *-------------------------------------------------------*/
  stc_dfa                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per oneri bancari                */
/*       *-------------------------------------------------------*/
  stc_onb                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per interessi bancari            */
/*       *-------------------------------------------------------*/
  stc_inb                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CIN italiano                                   */
/*       *-------------------------------------------------------*/
  cin_ita                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CIN europeo                                    */
/*       *-------------------------------------------------------*/
  cin_eur                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Default firma per autorizzazioni bancarie             */
/*       *-------------------------------------------------------*/
  fir_maa                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Default firma per operazioni bancarie                 */
/*       *-------------------------------------------------------*/
  fir_mao                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Percorso immagine                                     */
/*       *-------------------------------------------------------*/
  pth_img                   varchar (80)                not null,
/*       *=======================================================*/
/*       * Status commerciale banca                              */
/*       *                                                       */
/*       *  - 01 : Normale                                       */
/*       *  - 11 : Esauriti i rapporti commerciali               */
/*       *  - 21 : Sostituita da ns. nuova banca                 */
/*       *  - 51 : Cessata attivita'                             */
/*       *  - 52 : Cessata attivita', ma sostituita da ns.       */
/*       *         nuova banca                                   */
/*       *  - 61 : In contenzioso                                */
/*       *  - 62 : In contenzioso, ma sostituita da ns. nuova    */
/*       *         banca                                         */
/*       *  - 71 : Fallita                                       */
/*       *  - 72 : Fallita, ma sostituita da nostra nuova banca  */
/*       *-------------------------------------------------------*/
  sta_tus                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di determinazione dello status                   */
/*       *                                                       */
/*       * Solo se status diverso da 01                          */
/*       *-------------------------------------------------------*/
  sta_tud                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice banca di riferimento in relazione allo         */
/*       * status della banca, ovvero codice della nuova banca   */
/*       *                                                       */
/*       * Solo se status a 21,52,62,72                          */
/*       *-------------------------------------------------------*/
  sta_tuc                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di trattamento nelle statistiche per i      */
/*       * documenti precedenti il cambiamento di status         */
/*       *                                                       */
/*       * Solo se codice banca di riferimento diverso da        */
/*       * spazi                                                 */
/*       *                                                       */
/*       *  - 00 : Non significativo                             */
/*       *  - 01 : Statistiche distinte                          */
/*       *  - 02 : Statistiche girate sulla banca di riferimento */
/*       *=======================================================*/
  sta_tux                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo fido/castelletto concesso                     */
/*       *-------------------------------------------------------*/
  imp_fid                   bigint (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di scadenza dell'affidamento (opzionale)          */
/*       *-------------------------------------------------------*/
  sca_fid                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
