/*    *==========================================================*/
/*    * Tabella [uac] Utenti - Aziende - Comandi           {xpg} */
/*    *                                                          */
/*    * Aggiornamento 08.09.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_ute,
                                                        cod_azi,
                                                        cod_abl),
/*       *=======================================================*/
/*       * Codice utente                                         */
/*       *-------------------------------------------------------*/
  cod_ute                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice azienda                                        */
/*       *-------------------------------------------------------*/
  cod_azi                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice elemento abilitazione                          */
/*       *-------------------------------------------------------*/
  cod_abl                   varchar  (06)               not null,
/*       *-------------------------------------------------------*/
/*       * Tipo abilitazione                                     */
/*       *                                                       */
/*       * -  +  : Abilitato                                     */
/*       * -  -  : Non abilitato                                 */
/*       * -  v  : Visualizzazione solo                          */
/*       *-------------------------------------------------------*/
  snv_abl                   varchar  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Livello di abilitazione                               */
/*       *                                                       */
/*       *  Possibili definizioni        Esempi              Old */
/*       *  --------------------------- ----------           --- */
/*       *  -  0  : Sistema applicativo (swd,pgm,[su misura])(X) */
/*       *  -  1  : Gruppo gestionale   (ge,am,ve)           (G) */
/*       *  -  2  : Area gestionale     (xpg,cge)            (A) */
/*       *  -  3  : Settore gestionale  (cgemov)             (S) */
/*       *  -  4  : Fase gestionale     (cge300)             (F) */
/*       *-------------------------------------------------------*/
  liv_abl                   tinyint  (01)               not null
