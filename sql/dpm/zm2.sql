/*    *==========================================================*/
/*    * Tabella [zp2] Gruppi materie prime                 {dpm} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cla,
                                                        cod_gru),
/*       *=======================================================*/
/*       * Codice classe                                         */
/*       *-------------------------------------------------------*/
  cod_cla                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice gruppo                                         */
/*       *-------------------------------------------------------*/
  cod_gru                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione gruppo                                    */
/*       *-------------------------------------------------------*/
  des_gru                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico gruppo                               */
/*       *-------------------------------------------------------*/
  mne_gru                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione in uppercase per il gruppo                */
/*       *-------------------------------------------------------*/
  des_key                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Sequenza numerica                                     */
/*       *-------------------------------------------------------*/
  sqz_num                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gruppo suddiviso in sottogruppi            */
/*       *                                                       */
/*       * - 01 : No, il gruppo non e' suddiviso in sottogruppi  */
/*       * - 02 : Si, il gruppo e' suddiviso in sottogruppi      */
/*       *-------------------------------------------------------*/
  ult_sud                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura da proporre per gli elementi appar-  */
/*       * tenenti a questo gruppo.                              */
/*       *                                                       */
/*       * N.B. : Se il gruppo e' suddiviso in sottogruppi, que- */
/*       *        sta unita' di misura verra' proposta nella co- */
/*       *        difica dei sottogruppi appartenenti a questa   */
/*       *        classe.                                        */
/*       *                                                       */
/*       *        Se il gruppo non e' suddivisa in sottogruppi,  */
/*       *        questa unita' di misura verra' proposta nella  */
/*       *        codifica delle materie prime appartenenti a    */
/*       *        questo gruppo.                                 */
/*       *                                                       */
/*       *        Deve trovare corrispondenza nella tabella      */
/*       *        delle unita' di misura.                        */
/*       *-------------------------------------------------------*/
  umi_def                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Ultimo codice utilizzato nella codifica automatica    */
/*       * dei sottogruppi appartenenti a questo gruppo          */
/*       *-------------------------------------------------------*/
  ult_cod                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Percorso immagine                                     */
/*       *-------------------------------------------------------*/
  pth_img                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Colore gruppo                                         */
/*       *-------------------------------------------------------*/
  col_gru                   varchar (10)                not null
