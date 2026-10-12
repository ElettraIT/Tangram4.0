/*    *==========================================================*/
/*    * Tabella [zoc] Tipi movimento ordini clienti        {orc} */
/*    *                                                          */
/*    * Aggiornamento 02.08.2025                                 */
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
/*       *   - 01 : Emissione automatica da commissioni clienti  */
/*       *-------------------------------------------------------*/
  prv_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla numerazione per il documento associato al tipo  */
/*       * movimento                                             */
/*       *-------------------------------------------------------*/
  sgl_num                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo documento associato al tipo movimen- */
/*       * to per la stampa                                      */
/*       *-------------------------------------------------------*/
  des_stp                   varchar (30)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa prezzo                                   */
/*       *                                                       */
/*       * - 01 : Si                                             */
/*       * - 02 : No                                             */
/*       *-------------------------------------------------------*/
  snx_prz                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa sconti                                   */
/*       *                                                       */
/*       * - 01 : Si                                             */
/*       * - 02 : No                                             */
/*       *-------------------------------------------------------*/
  snx_sco                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa data consegna                            */
/*       *                                                       */
/*       * - 01 : Si                                             */
/*       * - 02 : No                                             */
/*       *-------------------------------------------------------*/
  snx_dtc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa totale ordine                            */
/*       *                                                       */
/*       * - 'N' : No                                            */
/*       * - 'S' : Si, totale merce                              */
/*       * - 'I' : Si, totale imponibile                         */
/*       * - 'X' : Si, totale merce, sconto e netto              */
/*       *-------------------------------------------------------*/
  snx_sto                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Default per accettazione tipo riga corpo              */
/*       *-------------------------------------------------------*/
  def_tpr                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa riferimento all'Agente                   */
/*       *                                                       */
/*       *  - 'S' : Codice e Nominativo dell'Agente              */
/*       *  - 'N' : Nessun riferimento all'Agente                */
/*       *  - 'C' : Solo il codice dell'Agente                   */
/*       *  - 'T' : Solo il nominativo dell'Agente               */
/*       *-------------------------------------------------------*/
  snx_age                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di ordine                                        */
/*       *                                                       */
/*       * - Spaces : normale                                    */
/*       * - P      : pro-forma (non impegna il magazzino)       */
/*       * - F      : forecast (previsionale)                    */
/*       *-------------------------------------------------------*/
  tip_ord                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio automaticamente proposto come default   */
/*       * fault per il tipo movimento                           */
/*       *                                                       */
/*       * - C      : Cliente                                    */
/*       * - D      : Dipendenza                                 */
/*       *-------------------------------------------------------*/
  def_tar                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa dicitura                                 */
/*       *                                                       */
/*       * - 01 : Si                                             */
/*       * - 02 : No                                             */
/*       *-------------------------------------------------------*/
  snx_dct                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione dicitura                                  */
/*       *                                                       */
/*       * N.B.: solo se campo precedente a 01                   */
/*       *-------------------------------------------------------*/
  des_dct                   varchar (240)               not null,
/*       *-------------------------------------------------------*/
/*       * Default tipo vendita per l'agente                     */
/*       *-------------------------------------------------------*/
  dtp_vpa                   tinyint (02)                not null,
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
