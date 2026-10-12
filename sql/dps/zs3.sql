/*    *==========================================================*/
/*    * Tabella [zs3] Sottogruppi semilavorati             {dps} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cla,
                                                        cod_gru,
                                                        cod_sgr),
/*       *=======================================================*/
/*       * Codice classe                                         */
/*       *-------------------------------------------------------*/
  cod_cla                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice gruppo                                         */
/*       *-------------------------------------------------------*/
  cod_gru                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice sottogruppo                                    */
/*       *-------------------------------------------------------*/
  cod_sgr                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione sottogruppo                               */
/*       *-------------------------------------------------------*/
  des_sgr                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico sottogruppo                          */
/*       *-------------------------------------------------------*/
  mne_sgr                   varchar (05)                not null,
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
/*       * - 01 : No, il sottogruppo non e' suddiviso            */
/*       * - 02 : Si, il sottogruppo e' suddiviso                */
/*       *                                                       */
/*       * N.B. : questo campo non e' ovviamente gestito, ed e'  */
/*       *        sempre posto al valore zero; e' stato inserito */
/*       *        nel record per analogia con i records delle    */
/*       *        classi e dei gruppi merceologici               */
/*       *-------------------------------------------------------*/
  ult_sud                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura da proporre per gli elementi appar-  */
/*       * tenenti a questo sottogruppo                          */
/*       *                                                       */
/*       * N.B. : Questa unita' di misura verra' proposta auto-  */
/*       *        maticamente nella codifica semilavorati ap-    */
/*       *        partenenti a questo sottogruppo                */
/*       *-------------------------------------------------------*/
  umi_def                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Ultimo codice utilizzato nella codifica automatica    */
/*       * dei sottogruppi appartenenti a questo gruppo          */
/*       *                                                       */
/*       * N.B. : questo campo non e' ovviamente gestito, ed e'  */
/*       *        sempre posto al valore zero; e' stato inserito */
/*       *        nel record per analogia con i records delle    */
/*       *        classi e dei gruppi merceologici               */
/*       *-------------------------------------------------------*/
  ult_cod                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Percorso immagine                                     */
/*       *-------------------------------------------------------*/
  pth_img                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Colore sottogruppo                                    */
/*       *-------------------------------------------------------*/
  col_sgr                   varchar (10)                not null
