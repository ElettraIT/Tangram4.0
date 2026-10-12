/*    *==========================================================*/
/*    * Tabella [zv1] Classi materiali vari                {mtv} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cla),
/*       *=======================================================*/
/*       * Codice classe                                         */
/*       *-------------------------------------------------------*/
  cod_cla                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione classe                                    */
/*       *-------------------------------------------------------*/
  des_cla                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico classe                               */
/*       *-------------------------------------------------------*/
  mne_cla                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione in uppercase per la classe                */
/*       *-------------------------------------------------------*/
  des_key                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Sequenza numerica                                     */
/*       *-------------------------------------------------------*/
  sqz_num                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di classe suddivisa in gruppi                 */
/*       *                                                       */
/*       * - 01 : No, la classe non e' suddivisa in gruppi       */
/*       * - 02 : Si, la classe e' suddivisa in gruppi           */
/*       *-------------------------------------------------------*/
  ult_sud                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura da proporre per gli elementi appar-  */
/*       * tenenti a questa classe.                              */
/*       *                                                       */
/*       * N.B. : Se la classe e' suddivisa in gruppi, questa u- */
/*       *        nita' di misura verra' proposta nella codifica */
/*       *        dei gruppi appartenenti a questa classe.       */
/*       *                                                       */
/*       *        Se la classe non e' suddivisa in gruppi, que-  */
/*       *        sta unita' di misura verra' proposta nella co- */
/*       *        difica delle materie varie appartenenti a      */
/*       *        questa classe.                                 */
/*       *                                                       */
/*       *        Deve trovare corrispondenza nella tabella      */
/*       *        delle unita' di misura.                        */
/*       *-------------------------------------------------------*/
  umi_def                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Ultimo codice utilizzato nella codifica automatica    */
/*       * dei gruppi appartenenti a questa classe               */
/*       *-------------------------------------------------------*/
  ult_cod                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Percorso immagine                                     */
/*       *-------------------------------------------------------*/
  pth_img                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Colore classe                                         */
/*       *-------------------------------------------------------*/
  col_cla                   varchar (10)                not null
