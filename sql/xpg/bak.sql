/*    *==========================================================*/
/*    * Tabella [bak] Procedure di back-up                 {xpg} */
/*    *                                                          */
/*    * Vedi anche [ebk] : elementi back-up                      */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_bak),
/*       *=======================================================*/
/*       * Codice procedura di salvataggio                       */
/*       *-------------------------------------------------------*/
  cod_bak                   varchar  (20)               not null,
/*       *-------------------------------------------------------*/
/*       * Tipo procedura di salvataggio                         */
/*       *                                                       */
/*       * Nota : Questo campo attualmente non e' gestito,       */
/*       *        ed il suo valore viene forzato a 1             */
/*       *-------------------------------------------------------*/
  tip_bak                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione procedura di salvataggio                  */
/*       *-------------------------------------------------------*/
  des_bak                   varchar  (80)               not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di utente abilitato                              */
/*       *                                                       */
/*       * - 01 : Tutti                                          */
/*       * - 02 : Solo l'utente supervisore                      */
/*       *-------------------------------------------------------*/
  tut_abl                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Password                                              */
/*       *-------------------------------------------------------*/
  pwd_bak                   varchar  (08)               not null,   
/*       *-------------------------------------------------------*/
/*       * Status della procedura                                */
/*       *                                                       */
/*       * - 00 : Puo' essere eseguita                           */
/*       * - 01 : Bloccata                                       */
/*       *-------------------------------------------------------*/
  sts_bak                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di unita' da utilizzare                          */
/*       *                                                       */
/*       * - 01 : Unita' per backup                              */
/*       * - 02 : Unita' per aggiornamenti software              */
/*       * - 03 : Unita' per floppy disk                         */
/*       * - 99 : Unita' speciale                                */
/*       * - 55 : Procedura di esecuzione batch                  */
/*       *-------------------------------------------------------*/
  tip_dev                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Pathname del device driver se unita' speciale         */
/*       *                                                       */
/*       *  oppure                                               */
/*       *                                                       */
/*       * Nome della procedura batch, che deve trovare cor-     */
/*       * rispondenza all'interno della directory specifica     */
/*       * '/abd/bat', in caso di procedura di esecuzione        */
/*       * batch                                                 */
/*       *                                                       */
/*       *  oppure                                               */
/*       *                                                       */
/*       * Spaces in tutti gli altri casi                        */
/*       *-------------------------------------------------------*/
  pth_uns                   varchar  (80)               not null,
/*       *-------------------------------------------------------*/
/*       * Pathname della directory su cui posizionarsi prima    */
/*       * dell'esecuzione della procedura                       */
/*       *                                                       */
/*       * In caso di procedura di esecuzione batch questo       */
/*       * campo e' posto a spaces                               */
/*       *-------------------------------------------------------*/
  pth_dir                   varchar  (80)               not null,
/*       *-------------------------------------------------------*/
/*       * Numero elementi della directory da trattare           */
/*       *                                                       */
/*       * In caso di procedura di esecuzione batch questo       */
/*       * campo e' posto a zero                                 */
/*       *-------------------------------------------------------*/
  num_ele                   tinyint  (02)               not null