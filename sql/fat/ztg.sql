/*    *==========================================================*/
/*    * Tabella [ztg] Codici TAG fattura elettronica       {fat} */
/*    *                                                          */
/*    * Aggiornamento 27.11.2025                                 */
/*    *                                                          */
/*    * ___ DA IMPLEMENTARE IN XPG700 ___                        */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                           (cod_tag),
/*       *=======================================================*/
/*       * Codice Tag                                            */
/*       *                                                       */
/*       * Formato: 9.9.9.9.99..                                 */
/*       *-------------------------------------------------------*/
  cod_tag                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione Tag                                       */
/*       *-------------------------------------------------------*/
  des_tag                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Commento al Tag                                       */
/*       *-------------------------------------------------------*/
  com_tag                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no Tag con sotto-nodi o Tag sotto-nodo             */
/*       *                                                       */
/*       * 'S' : Si, tag con sotto-nodi                          */
/*       * 'N' : No, tag senza sotto-nodi                        */
/*       *                                                       */
/*       * 'X' : Si, tag con sotto-nodi ma a sua volta sotto-    */
/*       *           nodo                                        */
/*       * 'Z' : No, tag senza sotto-nodi, ma sotto-nodo         */
/*       *                                                       */
/*       * N.B.: attualmente implementati solo 'S' e 'N'         */
/*       *-------------------------------------------------------*/
  sns_tag                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no Tag gestito                                     */
/*       *                                                       */
/*       * 'S' : Si, tag gestito                                 */
/*       * 'N' : No, tag non gestito                             */
/*       *-------------------------------------------------------*/
  sng_tag                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no Tag multiplo                                    */
/*       *                                                       */
/*       * 'N' : No, tag singolo                                 */
/*       * 'S' : Si, tag multiplo                                */
/*       *-------------------------------------------------------*/
  snx_mul                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione Tag per la stampa                         */
/*       *-------------------------------------------------------*/
  des_stp                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice Tag di appartenenza del nodo                   */
/*       *                                                       */
/*       * Formato: 9.9.9.9.99..                                 */
/*       *                                                       */
/*       * N.B.: solo se sotto-nodo                              */
/*       *-------------------------------------------------------*/
  cod_apn                    varchar (20)                not null


/*       *-------------------------------------------------------*/
/*       * Tipo di valore contenuto nel Tag                      */
/*       *                                                       */
/*       * spaces : Tag privo di valore                          */
/*       * 'TX'   : Testo                                        */
/*       * 'NU'   : Numero                                       */
/*       * 'VA'   : Valore                                       */
/*       * 'DA'   : Data                                         */
/*       * 'ND'   : Codice numerico da decodificare              */
/*       * 'AD'   : Codice alfanumerico da decodificare          */
/*       *                                                       */
/*       * N.B.: attualmente non implementato                    */
/*       *-------------------------------------------------------*/
