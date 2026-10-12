/*    *==========================================================*/
/*    * Tabella [yop] Iipi operazione per scadenziario     {scf} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_top),
/*       *=======================================================*/
/*       * Codice tipo operazione per gestione scadenze fornito- */
/*       * ri                                                    */
/*       *                                                       */
/*       * N.B. : La codifica dei tipi operazione non e' libera, */
/*       *        deve bensi' uniformarsi all'elenco presente    */
/*       *        nel programma di gestione 'scf300'             */
/*       *-------------------------------------------------------*/
  cod_top                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico per il tipo operazione               */
/*       *-------------------------------------------------------*/
  cod_mne                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione in uppercase                              */
/*       *-------------------------------------------------------*/
  des_key                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo operazione per gestione portafoglio  */
/*       *-------------------------------------------------------*/
  des_top                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale contabile per il tipo operazione       */
/*       *-------------------------------------------------------*/
  cau_cge                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice sottoconto contabile per il tipo operazione    */
/*       *-------------------------------------------------------*/
  stc_cge                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita contabile per il tipo operazione */
/*       *-------------------------------------------------------*/
  ctp_cge                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
