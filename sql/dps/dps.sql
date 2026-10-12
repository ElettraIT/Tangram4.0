/*    *==========================================================*/
/*    * Tabella [dps] Semilavorati                         {dps} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_sem),
/*       *-------------------------------------------------------*/
/*       * Chiavi addizionali                                    */
/*       *-------------------------------------------------------*/
  INDEX                     ALFSEM                     (alf_sem,
                                                        num_sem),
/*       *=======================================================*/
/*       * Codice numerico semilavorato                          */
/*       *-------------------------------------------------------*/
  num_sem                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice semilavorato alfanumerico                      */
/*       *-------------------------------------------------------*/
  alf_sem                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Sinonimo semilavorato                                 */
/*       *-------------------------------------------------------*/
  syn_sem                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Chiave libera semilavorato                            */
/*       *-------------------------------------------------------*/
  klb_sem                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione semilavorato, in uppercase                */
/*       *-------------------------------------------------------*/
  des_key                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione semilavorato                              */
/*       *-------------------------------------------------------*/
  des_sem                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Classificazione merceologica semilavorato             */
/*       *-------------------------------------------------------*/
  cla_sem                   integer (05)                not null,
  gru_sem                   integer (05)                not null,
  sgr_sem                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo semilavorato                                     */
/*       *                                                       */
/*       * - 00 : Semilavorato intermedio di un ciclo di lavora- */
/*       *        zione                                          */
/*       * - 01 : Semilavorato finale di un ciclo di lavorazione */
/*       *-------------------------------------------------------*/
  tip_sem                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No semilavorato in confezione                      */
/*       *                                                       */
/*       *  - 00 : No, la semilavorato non e' una confezione     */
/*       *  - 01 : Si, la semilavorato e' una confezione         */
/*       *                                                       */
/*       * N.B. : Queste informazioni non sono gestite dal pac-  */
/*       *        chetto standard, pero' sono a disposizione in  */
/*       *        caso di implementazioni personalizzate         */
/*       *-------------------------------------------------------*/
  tip_cfz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' per confezione                              */
/*       *                                                       */
/*       * Solo se Si/No semilavorato in confezione a 01         */
/*       *-------------------------------------------------------*/
  qta_cfz                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Peso unitario semilavorato, lordo                     */
/*       *-------------------------------------------------------*/
  pes_uni                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Peso tara semilavorato                                */
/*       *-------------------------------------------------------*/
  pes_tar                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Volume unitario semilavorato                          */
/*       *-------------------------------------------------------*/
  vol_uni                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Dimensioni semilavorato                               */
/*       *                                                       */
/*       * - Larghezza                                           */
/*       * - Altezza                                             */
/*       * - Profondita'                                         */
/*       *-------------------------------------------------------*/
  dim_lar                   decimal (10,03)             not null,
  dim_alt                   decimal (10,03)             not null,
  dim_prf                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Peculiarita' fisiche specifiche semilavorato          */
/*       *-------------------------------------------------------*/
  pcl_fis                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Coefficiente moltiplicatore libero                    */
/*       *-------------------------------------------------------*/
  coe_mol                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Coefficiente divisore libero                          */
/*       *-------------------------------------------------------*/
  coe_div                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura per la produzione                    */
/*       *-------------------------------------------------------*/
  umi_prd                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali quantita' per la produzione : 0..3    */
/*       *-------------------------------------------------------*/
  dec_qta                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della seconda quantita' nella     */
/*       * produzione                                            */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_2qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la seconda quantita'              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_2qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della terza quantita' nella       */
/*       * produzione                                            */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_3qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la terza quantita'                */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_3qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo variante per la produzione                       */
/*       *-------------------------------------------------------*/
  tip_vpr                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codici statistici semilavorato                        */
/*       *-------------------------------------------------------*/
  cod_s01                   integer (05)                not null,
  cod_s02                   integer (05)                not null,
  cod_s03                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di inizio impiego semilavorato                   */
/*       *-------------------------------------------------------*/
  dat_iim                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Status semilavorato                                   */
/*       *                                                       */
/*       *  - 01 : Normale                                       */
/*       *  - 11 : Ad esaurimento                                */
/*       *  - 21 : Sostituita da nuovo codice                    */
/*       *  - 51 : Cessato acquisto                              */
/*       *  - 52 : Cessato acquisto, ma sostituito da nuovo      */
/*       *         codice                                        */
/*       *  - 71 : Obsoleta                                      */
/*       *  - 72 : Obsoleta, ma sostituito da nuovo codice       */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  sta_tus                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di determinazione dello status                   */
/*       *                                                       */
/*       * Solo se status diverso da 01                          */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  sta_tud                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice numerico materia prima di riferimento in rela- */
/*       * zione allo status, ovvero codice numerico del nuovo   */
/*       * semilavorato                                          */
/*       *                                                       */
/*       * Solo se status a 21,52,72                             */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  sta_tuc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di trattamento nelle statistiche per i      */
/*       * documenti precedenti il cambiamento di status         */
/*       *                                                       */
/*       * Solo se semilavorato di riferimento diverso da zero   */
/*       *                                                       */
/*       *  - 00 : Non significativo                             */
/*       *  - 01 : Statistiche distinte                          */
/*       *  - 02 : Statistiche girate su semilavorato di         */
/*       *         riferimento                                   */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  sta_tux                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice classe di budget per il semilavorato           */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  cla_bdg                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di semilavorato con accettazione ordini       */
/*       * bloccata                                              */
/*       *                                                       */
/*       * - 01 : No, ordini accettabili                         */
/*       * - 02 : Si, ordini non accettabili                     */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  for_blo                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data in cui e' avvenuto il blocco/sblocco della       */
/*       * accettabilita' degli ordini                           */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dor_blo                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice della nomenclatura combinata della merce per   */
/*       * la gestione IVA Intracomunitaria                      */
/*       *                                                       */
/*       * PENSARE A CODICI 'ISO'                                */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  cdn_cdm                   integer (08)                not null,
/*       *-------------------------------------------------------*/
/*       * Percorso immagine                                     */
/*       *-------------------------------------------------------*/
  pth_img                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
