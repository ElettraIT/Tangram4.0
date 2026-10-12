/*    *==========================================================*/
/*    * Tabella [zbi] Tipi movimento bolle di vendita      {bol} */
/*    *                                                          */
/*    * Aggiornamento 10.06.2026                                 */
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
/*       * Si/No interessa la fatturazione                       */
/*       *                                                       */
/*       * - 01 : No                                             */
/*       * - 02 : Si, il documento sara' in seguito sottoposto a */
/*       *            fatturazione                               */
/*       * - 03 : Si, il documento stesso e' anche una fattura   */
/*       *-------------------------------------------------------*/
  int_ftr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento di fatturazione                        */
/*       *                                                       */
/*       * - Se movimento che interessa la fatturazione 01       */
/*       *      - Valore fisso Spaces, non significativo         */
/*       *                                                       */
/*       * - Se movimento che interessa la fatturazione 02       */
/*       *      - Codice tipo movimento di fatturazione che      */
/*       *        sara' utilizzato dalla procedura di fat-       */
/*       *        turazione automatica da bolle emesse           */
/*       *                                                       */
/*       * - Se movimento che interessa la fatturazione 03       */
/*       *      - Codice tipo movimento di fatturazione cor-     */
/*       *        rispondente alla componente 'fattura' del      */
/*       *        presente tipo movimento di bollettazione       */
/*       *-------------------------------------------------------*/
  tmo_ftr                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No documento che accompagna la merce               */
/*       *-------------------------------------------------------*/
  snx_acm                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Default per trasporto a cura di                       */
/*       *                                                       */
/*       * - 01     : Nessun default                             */
/*       * - 10     : Mittente                                   */
/*       * - 20     : Destinatario                               */
/*       * - 30     : Vettore                                    */
/*       *-------------------------------------------------------*/
  def_tac                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Default per la causale del trasporto                  */
/*       *-------------------------------------------------------*/
  def_ctr                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale per movimento di magazzino             */
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
/*       * N.B. : Solo se codice causale per movimento di        */
/*       *        magazzino diverso da zero                      */
/*       *                                                       */
/*       *        Solo causale di magazzino che riguarda         */
/*       *        movimentazione di conto merce                  */
/*       *-------------------------------------------------------*/
  cod_mic                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale di magazzino aggiuntiva                */
/*       *                                                       */
/*       * N.B. : Solo per i tipi movimento di trasferimento a   */
/*       *        dipendenza                                     */
/*       *-------------------------------------------------------*/
  cam_agg                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio automaticamente proposto come default   */
/*       * fault per il tipo movimento                           */
/*       *                                                       */
/*       * - Spaces : Nessun tipo archivio proposto              */
/*       * - N      : No tipo archivio                           */
/*       * - C      : Cliente                                    */
/*       * - F      : Fornitore                                  */
/*       * - D      : Dipendenza                                 */
/*       * - A      : Agente (Incaricato alla vendita)           */
/*       *                                                       */
/*       * N.B. : Se il documento interessa la fatturazione, il  */
/*       *        il tipo archivio deve essere pari a 'C'        */
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
/*       * Sigla numerazione per il documento associato al tipo  */
/*       * movimento                                             */
/*       *-------------------------------------------------------*/
  sgl_num                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Movimento a fronte di :                               */
/*       *                                                       */
/*       * - 01 : Niente                                         */
/*       * - 02 : Ordine cliente                                 */
/*       * - 03 : Ordine di spedizione                           */
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
/*       * Descrizione tipo documento associato al tipo movimen- */
/*       * to per la stampa                                      */
/*       *-------------------------------------------------------*/
  des_stp                   varchar (30)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa                                          */
/*       *                                                       */
/*       * - 01 : Si                                             */
/*       * - 02 : No                                             */
/*       *                                                       */
/*       *        - Si/No stampa prezzo                          */
/*       *        - Si/No stampa sconti                          */
/*       *        - Si/No stampa importo                         */
/*       *        - Si/No stampa codice IVA                      */
/*       *        - Si/No stampa totale                          */
/*       *        - Si/No stampa dicitura fissa                  */
/*       *-------------------------------------------------------*/
  snx_prz                   tinyint (02)                not null,
  snx_sco                   tinyint (02)                not null,
  snx_imp                   tinyint (02)                not null,
  snx_civ                   tinyint (02)                not null,
  snx_ttd                   tinyint (02)                not null,
  snx_dct                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione dicitura                                  */
/*       *                                                       */
/*       * N.B.: solo se campo precedente a 01                   */
/*       *-------------------------------------------------------*/
  des_dct                   varchar (240)               not null,
/*       *-------------------------------------------------------*/
/*       * Numero copie da stampare                              */
/*       *-------------------------------------------------------*/
  num_cps                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Default tipo vendita per l'agente                     */
/*       *-------------------------------------------------------*/
  dtp_vpa                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no default data inizio trasporto                   */
/*       *-------------------------------------------------------*/
  snx_ddt                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Validita' per le dipendenze                           */
/*       *                                                       */
/*       *  - spaces : Valido per tutte le dipendenze            */
/*       *  - 'S'    : Valido solo per la Sede                   */
/*       *  - 'D'    : Valido solo per le dipendenze             */
/*       *  - 'X'    : Valido solo per le dipendenze indicate    */
/*       *-------------------------------------------------------*/
  vld_dpz                   varchar (01)                not null,
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
/*       * Si/No stampa riferimento alla nostra Dipendenza       */
/*       *-------------------------------------------------------*/
  snx_ndp                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa forma di pagamento                       */
/*       *-------------------------------------------------------*/
  snx_fop                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No controllo su numero copie gia' eseguite         */
/*       *                                                       */
/*       *  - spaces : Ristampa libera, senza controllo          */
/*       *  - 'S'    : Ristampa con controllo                    */
/*       *-------------------------------------------------------*/
  snx_cse                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Default per accettazione tipo riga corpo              */
/*       *-------------------------------------------------------*/
  def_tpr                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No numerazione modificabile                        */
/*       *-------------------------------------------------------*/
  snx_nmm                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento di fatturazione alternativo            */
/*       *-------------------------------------------------------*/
  tmo_ft2                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No numero copie variabile                          */
/*       *-------------------------------------------------------*/
  snx_ncv                   varchar (01)                not null,
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
