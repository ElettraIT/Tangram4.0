/*    *==========================================================*/
/*    * Tabella [lgr] Distinte base : righe                {dtp} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tpm_ass,
                                                        nrm_ass,
                                                        num_prg),
/*       *=======================================================*/
/*       * Tipo magazzino interessato per l'assieme              */
/*       *                                                       */
/*       * - 01 : Prodotti di vendita                            */
/*       * - 02 : Semilavorati                                   */
/*       * - 99 : Sub-distinte virtuali                          */
/*       *-------------------------------------------------------*/
  tpm_ass                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice numerico magazzino per l'assieme               */
/*       *                                                       */
/*       * - Se prodotto di vendita   : codice su [dcp]          */
/*       * - Se semilavorato          : codice su [dps]          */
/*       * - Se sub-distinta virtuale : codice su [lgv]          */
/*       *-------------------------------------------------------*/
  nrm_ass                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo elemento nella distinta            */
/*       *-------------------------------------------------------*/
  num_prg                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo magazzino interessato per il componente          */
/*       *                                                       */
/*       * - 02 : Semilavorati                                   */
/*       * - 03 : Materie prime                                  */
/*       * - 99 : Sub-distinte virtuali                          */
/*       *-------------------------------------------------------*/
  tpm_cpt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice numerico magazzino per il componente           */
/*       *                                                       */
/*       * - Se semilavorato          : codice su [dps]          */
/*       * - Se materia prima         : codice su [dpm]          */
/*       * - Se sub-distinta virtuale : codice su [lgv]          */
/*       *-------------------------------------------------------*/
  nrm_cpt                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice alfanumerico magazzino per il componente       */
/*       * - Se semilavorato          : codice su [dps]          */
/*       * - Se materia prima         : codice su [dpm]          */
/*       * - Se sub-distinta virtuale : codice su [lgv]          */
/*       *-------------------------------------------------------*/
  afm_cpt                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura per la produzione                    */
/*       *                                                       */
/*       * N.B.: Campo non gestito                               */
/*       *-------------------------------------------------------*/
  umi_prd                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la quantita' di impiego : 0..3    */
/*       *                                                       */
/*       * N.B.: Il valore viene forzato sempre al valore conte- */
/*       *       nuto nel relativo record anagrafico             */
/*       *-------------------------------------------------------*/
  dec_qta                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' di impiego, moltiplicatore                  */
/*       *-------------------------------------------------------*/
  qta_imp                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' di impiego, divisore                        */
/*       *-------------------------------------------------------*/
  qta_ipd                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Note relative al componente                           */
/*       *-------------------------------------------------------*/
  not_cpt                   varchar (80)                not null