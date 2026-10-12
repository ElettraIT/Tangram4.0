/*    *==========================================================*/
/*    * Tabella [lgt] Distinte base : testate              {dtp} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tpm_ass,
                                                        nrm_ass),
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
/*       * Codice alfanumerico magazzino per l'assieme           */
/*       *                                                       */
/*       * - Se prodotto di vendita   : codice su [dcp]          */
/*       * - Se semilavorato          : codice su [dps]          */
/*       * - Se sub-distinta virtuale : codice su [lgv]          */
/*       *-------------------------------------------------------*/
  afm_ass                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla del disegno                                     */
/*       *-------------------------------------------------------*/
  sgl_dsg                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Commento                                              */
/*       *-------------------------------------------------------*/
  com_ass                   varchar (120)               not null,
/*       *-------------------------------------------------------*/
/*       *  % di scarto in fase di produzione                    */
/*       *-------------------------------------------------------*/
  prc_sct                   decimal (06,03)             not null,
/*       *-------------------------------------------------------*/
/*       *  % di rilavorazione in fase di produzione             */
/*       *-------------------------------------------------------*/
  prc_rlv                   decimal (06,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null

