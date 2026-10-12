/*    *==========================================================*/
/*    * Tabella [dpm] Materie prime                        {dpm} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_map),
/*       *-------------------------------------------------------*/
/*       * Chiavi addizionali                                    */
/*       *-------------------------------------------------------*/
  INDEX                     ALFMAP                     (alf_map,
                                                        num_map),
/*       *=======================================================*/
/*       * Codice numerico materia prima                         */
/*       *-------------------------------------------------------*/
  num_map                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice materia prima alfanumerico                     */
/*       *-------------------------------------------------------*/
  alf_map                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Sinonimo materia prima                                */
/*       *-------------------------------------------------------*/
  syn_map                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Chiave libera materia prima                           */
/*       *-------------------------------------------------------*/
  klb_map                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione materia prima, in uppercase               */
/*       *-------------------------------------------------------*/
  des_key                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione materia prima                             */
/*       *-------------------------------------------------------*/
  des_map                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Classificazione merceologica materia prima            */
/*       *-------------------------------------------------------*/
  cla_map                   integer (05)                not null,
  gru_map                   integer (05)                not null,
  sgr_map                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo materia prima                                    */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  tip_map                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No materia prima in confezione                     */
/*       *                                                       */
/*       *  - 00 : No, la materia prima non e' una confezione    */
/*       *  - 01 : Si, la materia prima e' una confezione        */
/*       *-------------------------------------------------------*/
  tip_cfz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' per confezione                              */
/*       *                                                       */
/*       * Solo se Si/No materia prima in confezione a 01        */
/*       *-------------------------------------------------------*/
  qta_cfz                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Peso unitario materia prima, lordo                    */
/*       *-------------------------------------------------------*/
  pes_uni                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Peso tara materia prima                               */
/*       *-------------------------------------------------------*/
  pes_tar                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Volume unitario materia prima                         */
/*       *-------------------------------------------------------*/
  vol_uni                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Dimensioni materia prima                              */
/*       *                                                       */
/*       * - Larghezza                                           */
/*       * - Altezza                                             */
/*       * - Profondita'                                         */
/*       *-------------------------------------------------------*/
  dim_lar                   decimal (10,03)             not null,
  dim_alt                   decimal (10,03)             not null,
  dim_prf                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Peculiarita' fisiche specifiche materia prima         */
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
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  tip_vpr                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codici statistici materia prima                       */
/*       *-------------------------------------------------------*/
  cod_s01                   integer (05)                not null,
  cod_s02                   integer (05)                not null,
  cod_s03                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di inizio impiego materia prima                  */
/*       *-------------------------------------------------------*/
  dat_iim                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Status materia prima                                  */
/*       *                                                       */
/*       *  - 01 : Normale                                       */
/*       *  - 11 : Ad esaurimento                                */
/*       *  - 21 : Sostituita da nuovo codice                    */
/*       *  - 51 : Cessato acquisto                              */
/*       *  - 52 : Cessato acquisto, ma sostituita da nuovo      */
/*       *         codice                                        */
/*       *  - 71 : Obsoleta                                      */
/*       *  - 72 : Obsoleta, ma sostituito da nuovo codice       */
/*       *-------------------------------------------------------*/
  sta_tus                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di determinazione dello status                   */
/*       *                                                       */
/*       * Solo se status diverso da 01                          */
/*       *-------------------------------------------------------*/
  sta_tud                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice numerico materia prima di riferimento in rela- */
/*       * zione allo status, ovvero codice numerico della nuova */
/*       * materia prima                                         */
/*       *                                                       */
/*       * Solo se status a 21,52,72                             */
/*       *-------------------------------------------------------*/
  sta_tuc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di trattamento nelle statistiche per i      */
/*       * documenti precedenti il cambiamento di status         */
/*       *                                                       */
/*       * Solo se materia prima di riferimento diversa da zero  */
/*       *                                                       */
/*       *  - 00 : Non significativo                             */
/*       *  - 01 : Statistiche distinte                          */
/*       *  - 02 : Statistiche girate su materia prima di        */
/*       *         riferimento                                   */
/*       *-------------------------------------------------------*/
  sta_tux                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice classe di budget per la materia prima          */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  cla_bdg                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di materia prima con accettazione ordini      */
/*       * bloccata                                              */
/*       *                                                       */
/*       * - 01 : No, ordini accettabili                         */
/*       * - 02 : Si, ordini non accettabili                     */
/*       *-------------------------------------------------------*/
  for_blo                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data in cui e' avvenuto il blocco/sblocco della       */
/*       * accettabilita' degli ordini                           */
/*       *-------------------------------------------------------*/
  dor_blo                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice della nomenclatura combinata della merce per   */
/*       * la gestione IVA Intracomunitaria                      */
/*       *                                                       */
/*       * PENSARE A CODICI 'ISO'                                */
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

