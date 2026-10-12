/*    *==========================================================*/
/*    * Tabella [lgv] Sub-distinte virtuali                {dtp} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_lgv),
/*       *=======================================================*/
/*       * Codice sub-distinta virtuale numerico                 */
/*       *-------------------------------------------------------*/
  num_lgv                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice sub-distinta virtuale alfanumerico             */
/*       *-------------------------------------------------------*/
  alf_lgv                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Sinonimo sub-distinta virtuale                        */
/*       *-------------------------------------------------------*/
  syn_lgv                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione sub-distinta virtuale in uppercase        */
/*       *-------------------------------------------------------*/
  des_key                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione sub-distinta virtuale                     */
/*       *-------------------------------------------------------*/
  des_lgv                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo sub-distinta virtuale                            */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  tip_lgv                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura per la produzione                    */
/*       *-------------------------------------------------------*/
  umi_prd                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali quantita'                             */
/*       *-------------------------------------------------------*/
  dec_qta                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della seconda quantita' nella     */
/*       * fatturazione                                          */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_2qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la seconda quantita'              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_2qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della terza quantita' nella       */
/*       * fatturazione                                          */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_3qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la terza quantita'                */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_3qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo variante per la produzione                       */
/*       *-------------------------------------------------------*/
  tip_vpr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
