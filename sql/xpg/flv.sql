/*    *==========================================================*/
/*    * Tabella [flv] Valori campi tabelle                 {xpg} */
/*    *                                                          */
/*    * Estensione della tabella [fld] per la gestione dei       */
/*    * valori codificati / precablati associati ai campi.       */
/*    *                                                          */
/*    * Esempi:                                                  */
/*    * - dcc.tip_esm                                            */
/*    * - dcc.dtt_acu                                            */
/*    * - iva.tip_iva                                            */
/*    *                                                          */
/*    * N.B. : La tabella contiene i valori ammessi del campo,   */
/*    *        ma NON le conseguenze gestionali legate alla      */
/*    *        scelta del valore stesso.                         */
/*    *                                                          */
/*    * Aggiornamento 19.04.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_tbl,
                                                        cod_fld,
                                                        cod_val),
/*       *=======================================================*/
/*       * Codice tabella                                        */
/*       *-------------------------------------------------------*/
  cod_tbl                   varchar  (10)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice campo                                          */
/*       *-------------------------------------------------------*/
  cod_fld                   varchar  (30)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice valore                                         */
/*       *                                                       */
/*       * N.B. : Campo alfanumerico per consentire la gestione  */
/*       *        sia di valori numerici (01, 02, 99) sia di     */
/*       *        valori alfabetici (A, B, D, E, N, S, ecc.).    */
/*       *-------------------------------------------------------*/
  cod_val                   varchar  (20)               not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione valore                                    */
/*       *-------------------------------------------------------*/
  des_val                   varchar  (120)              not null,
/*       *-------------------------------------------------------*/
/*       * Ordine di presentazione                               */
/*       *                                                       */
/*       * N.B. : Consente di stabilire l'ordine logico dei      */
/*       *        valori in select, radio, stampa, ecc.,         */
/*       *        indipendentemente dal contenuto di cod_val.    */
/*       *-------------------------------------------------------*/
  num_ord                   integer                     not null,
/*       *-------------------------------------------------------*/
/*       * Si/no valore attivo                                   */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Un valore non attivo puo' restare memorizzato  */
/*       *        nei dati storici ma non essere piu' proposto   */
/*       *        nei nuovi inserimenti.                         */
/*       *-------------------------------------------------------*/
  snx_att                   tinyint  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Si/no valore standard ERP                             */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Campo utile per distinguere i valori previsti  */
/*       *        originariamente dalla logica ERP da eventuali  */
/*       *        personalizzazioni successive.                  */
/*       *-------------------------------------------------------*/
  snx_std                   tinyint  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Help note                                             */
/*       *                                                       */
/*       * N.B. : Annotazione opzionale utile per spiegare il    */
/*       *        significato del valore o eventuali note di     */
/*       *        utilizzo in fase di manutenzione.              */
/*       *-------------------------------------------------------*/
  hlp_val                   varchar  (255)              not null
