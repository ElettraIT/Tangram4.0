/*    *==========================================================*/
/*    * Tabella [gxs] Stradari citta' italiane             {fdb} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (sgl_std,
                                                        des_key,
                                                        spc_key),
/*       *=======================================================*/
/*       * Sigla stradario                                       */
/*       *-------------------------------------------------------*/
  sgl_std                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione in chiave per la strada                   */
/*       *                                                       */
/*       * N.B.   : L'utente dovra' riempire questo campo con    */
/*       *          il nome piu' significativo, o i nomi piu'    */
/*       *          significativi, per la strada.                */
/*       *                                                       */
/*       * Esempi : BATTISTI                                     */
/*       *          GIULIO CESARE                                */
/*       *          OBERDAN                                      */
/*       *-------------------------------------------------------*/
  des_key                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Ulteriore specifica in chiave per la strada           */
/*       *                                                       */
/*       * Esempi : VIA                                          */
/*       *          CORSO                                        */
/*       *          LARGO                                        */
/*       *          VIA,NUMERI PARI                              */
/*       *          CORSO,N. DAL 117 AL 425                      */
/*       *-------------------------------------------------------*/
  spc_key                   varchar  (40)               not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione reale per la strada                       */
/*       *                                                       */
/*       * Esempi : Via Cesare Battisti                          */
/*       *          Piazza Cesare Battisti                       */
/*       *          Corso Giulio Cesare                          */
/*       *          Largo Oberdan                                */
/*       *                                                       */
/*       *                                                       */
/*       * N.B.   : In caso di :                                 */
/*       *                                                       */
/*       *          - descrizione in chiave per la strada        */
/*       *          - ulteriore specifica in chiave per strada   */
/*       *                                                       */
/*       *          entrambi a spaces, questo campo indica il    */
/*       *          titolo per lo stradario                      */
/*       *-------------------------------------------------------*/
  des_str                   varchar  (40)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice di avviamento postale per la strada            */
/*       *-------------------------------------------------------*/
  cap_avp                   varchar  (05)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice comune di appartenenza per la strada           */
/*       *-------------------------------------------------------*/
  cod_cmn                   integer  (05)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice frazione di appartenenza per la strada         */
/*       *-------------------------------------------------------*/
  cod_fzn                   integer  (03)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice localita' di appartenenza per la strada        */
/*       *-------------------------------------------------------*/
  cod_lct                   integer  (03)               not null