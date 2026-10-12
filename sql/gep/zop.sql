/*    *==========================================================*/
/*    * Tabella [zop] Iipi operazione per il portafoglio   {gep} */
/*    *                                                          */
/*    * Aggiornamento 14.12.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_top),
/*       *=======================================================*/
/*       * Codice tipo operazione per gestione portafoglio       */
/*       *                                                       */
/*       * N.B. : La codifica dei tipi operazione non e' libera, */
/*       *        deve bensi' uniformarsi all'elenco seguente :  */
/*       *                                                       */
/*       * - 0101 : Emissione Rimessa diretta                    */
/*       * - 0102 : Emissione Incasso elettronico                */
/*       * - 0103 : Emissione Ri.Ba.                             */
/*       * - 0104 : Emissione C.d.O.                             */
/*       * - 0105 : Emissione M.Av.                              */
/*       * - 0106 : Emissione R.I.D.                             */
/*       * - 0107 : Emissione Bonifico bancario                  */
/*       * - 0108 : Emissione C/C postale                        */
/*       * - 0109 : Emissione Ricevuta bancaria                  */
/*       * - 0110 : Emissione Tratta                             */
/*       * - 0111 : Emissione Paghero' cambiario                 */
/*       *                                                       */
/*       * - 0161 : Cesione Paghero' Cambiario da Cliente        */
/*       *                                                       */
/*       * - 0200 : Storno Scadenza                              */
/*       *                                                       */
/*       * - 0301 : Riscossione scadenza per Contanti            */
/*       * - 0302 : Riscossione scadenza per Assegno             */
/*       * - 0303 : Riscossione scadenza per Bonifico Bancario   */
/*       * - 0304 : Riscossione scadenza per Bonifico in C/C Po- */
/*       *          stale                                        */
/*       *                                                       */
/*       * - 0401 : Composizione Distinta Incassi Elettronici    */
/*       * - 0402 : Composizione Distinta Effetti                */
/*       * - 0403 : Composizione Distinta Paghero' Cambiari      */
/*       * - 0404 : Composizione Distinta Cessioni               */
/*       *                                                       */
/*       * - 0501 : Presentazione Distinta SBF a Maturazione di  */
/*       *          Valuta                                       */
/*       * - 0502 : Presentazione Distinta SBF con Accredito Im- */
/*       *          mediato                                      */
/*       * - 0503 : Presentazione Distinta al Dopo Incasso       */
/*       * - 0504 : Presentazione Distinta allo Sconto           */
/*       *                                                       */
/*       * - 0510 : Preparazione Archivio di Supporto per Di-    */
/*       *          stinte                                       */
/*       *                                                       */
/*       * - 0515 : Stampa Distinta di Presentazione             */
/*       *                                                       */
/*       * - 0516 : Stampa Effetti a fronte Distinta             */
/*       *                                                       */
/*       * - 0520 : Riscontro Tipo Avviso a Debitori per Incassi */
/*       *          Elettronici                                  */
/*       *                                                       */
/*       * - 0540 : Storno Distinta presentata alla Banca        */
/*       *                                                       */
/*       * - 0560 : Accredito Distinta in C/Corrente Bancario    */
/*       *                                                       */
/*       * - 0600 : Insoluto su Scadenza presentata in Banca     */
/*       *                                                       */
/*       * - 0620 : Richiamo di Scadenza presentata in Banca     */
/*       *                                                       */
/*       * - 0700 : Accredito di Scadenza presentata al Dopo In- */
/*       *          casso                                        */
/*       *                                                       */
/*       * - 0720 : Notizia di buon esito Scadenze SBF o allo    */
/*       *          Sconto                                       */
/*       *                                                       */
/*       * - 0740 : Presunto buon esito di Scadenze SBF o allo   */
/*       *          Sconto                                       */
/*       *                                                       */
/*       * - 0741 : Presunto buon esito di Scadenze SBF o allo   */
/*       *          Sconto, per gruppi di scadenze               */
/*       *-------------------------------------------------------*/
  cod_top                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico per il tipo operazione               */
/*       *-------------------------------------------------------*/
  cod_mne                   varchar (05)                not null,
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
