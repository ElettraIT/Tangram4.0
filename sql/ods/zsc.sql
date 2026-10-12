/*    *==========================================================*/
/*    * Tabella [zsc] Tipi movimento di spedizione         {ods} */
/*    *                                                          */
/*    * Aggiornamento 04.11.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                           (cod_tmo),
/*       *=======================================================*/
/*       * Codice tipo movimento                                 */
/*       *-------------------------------------------------------*/
  cod_tmo                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo movimento per uso interno            */
/*       *-------------------------------------------------------*/
  des_tmo                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Password per il tipo movimento                        */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  pwd_tmo                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Validita' per le dipendenze                           */
/*       *                                                       */
/*       * - 01 : Documento valido per tutte le dipendenze       */
/*       * - 02 : Documento valido per una sola dipendenza       */
/*       *-------------------------------------------------------*/
  vld_dpz                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza per cui il documento e' valido      */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Movimento a fronte di :                               */
/*       *                                                       */
/*       * - 01 : Niente                                         */
/*       * - 02 : Ordine cliente                                 */
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
/*       * Tipo archivio per il tipo movimento                   */
/*       *                                                       */
/*       * - N      : No tipo archivio                           */
/*       * - C      : Cliente                                    */
/*       * - F      : Fornitore                                  */
/*       * - D      : Dipendenza                                 */
/*       *                                                       */
/*       * N.B. : Se movimento a fronte di ordine cliente, il    */
/*       *        tipo archivio deve essere pari a 'C'           */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento per bollettazione automatica           */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  tmo_btz                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Default per accettazione tipo riga corpo              */
/*       *-------------------------------------------------------*/
  def_tpr                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Default modulo da stampare                            */
/*       *-------------------------------------------------------*/
  def_mdf                   varchar (08)                not null,
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
