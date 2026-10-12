/*    *==========================================================*/
/*    * Tabella [zso] Tipi di sollecito                    {gep} */
/*    *                                                          */
/*    * Aggiornamento 29.03.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_tso),
/*       *=======================================================*/
/*       * Codice tipo di sollecito                              */
/*       *-------------------------------------------------------*/
  cod_tso                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico per il tipo di sollecito             */
/*       *-------------------------------------------------------*/
  cod_mne                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo sollecito                            */
/*       *-------------------------------------------------------*/
  des_tso                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Gruppo righe di testo per la testata                  */
/*       *-------------------------------------------------------*/
  rdt_t01                   varchar (120)               not null,
  rdt_t02                   varchar (120)               not null,
  rdt_t03                   varchar (120)               not null,
  rdt_t04                   varchar (120)               not null,
  rdt_t05                   varchar (120)               not null,
/*       *-------------------------------------------------------*/
/*       * Numero di interlinee di separazione tra la testata ed */
/*       * il corpo                                              */
/*       *-------------------------------------------------------*/
  ids_tes                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No applicazione spese di insoluto per il tipo di   */
/*       * sollecito                                             */
/*       *  - N : No                                             */
/*       *  - S : Si                                             */
/*       *-------------------------------------------------------*/
  snx_ads                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo unitario spese di insoluto, in valuta base    */
/*       *                                                       */
/*       * N.B. : Solo se Si/No applicazione spese S             */
/*       *-------------------------------------------------------*/
  imp_ads                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice iva spese di insoluto                          */
/*       *                                                       */
/*       * N.B. : Solo se Si/No applicazione spese S             */
/*       *-------------------------------------------------------*/
  civ_ads                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No applicazione interessi per il tipo di sollecito */
/*       *  - N : No                                             */
/*       *  - S : Si                                             */
/*       *-------------------------------------------------------*/
  snx_api                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di applicazione interessi per il tipo di solle-  */
/*       * cito                                                  */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito, in quanto il    */
/*       *        tipo di applicazione avviene in funzione del   */
/*       *        valore della personalizzazione con codice      */
/*       *        pgm/gep/slc[tip-api]                           */
/*       *                                                       */
/*       * N.B. : Solo se Si/No applicazione interessi S         */
/*       *-------------------------------------------------------*/
  tip_api                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tasso di interesse da applicare                       */
/*       *                                                       */
/*       * N.B. : Solo se Si/No applicazione interessi S         */
/*       *-------------------------------------------------------*/
  tas_api                   decimal (07,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa 'per conoscenza' all'agente              */
/*       *-------------------------------------------------------*/
  snx_pca                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di interlinee di separazione tra il corpo e il */
/*       * riepilogo                                             */
/*       *                                                       */
/*       * N.B. : Solo se Si/No applicazione interessi S         */
/*       *-------------------------------------------------------*/
  ids_rie                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Gruppo righe di testo per il riepilogo                */
/*       *                                                       */
/*       * N.B. : Solo se Si/No applicazione interessi S         */
/*       *-------------------------------------------------------*/
  rdt_r01                   varchar (120)               not null,
  rdt_r02                   varchar (120)               not null,
  rdt_r03                   varchar (120)               not null,
  rdt_r04                   varchar (120)               not null,
  rdt_r05                   varchar (120)               not null,
/*       *-------------------------------------------------------*/
/*       * Numero di interlinee di separazione tra il corpo o il */
/*       * riepilogo ed il piede                                 */
/*       *-------------------------------------------------------*/
  ids_pie                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Gruppo righe di testo per il piede                    */
/*       *-------------------------------------------------------*/
  rdt_p01                   varchar (120)               not null,
  rdt_p02                   varchar (120)               not null,
  rdt_p03                   varchar (120)               not null,
  rdt_p04                   varchar (120)               not null,
  rdt_p05                   varchar (120)               not null,
  rdt_p06                   varchar (120)               not null,
  rdt_p07                   varchar (120)               not null,
  rdt_p08                   varchar (120)               not null,
  rdt_p09                   varchar (120)               not null,
  rdt_p10                   varchar (120)               not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
