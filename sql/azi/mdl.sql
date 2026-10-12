/*    *==========================================================*/
/*    * Tabella [mdl] Moduli di stampa                     {azi} */
/*    *                                                          */
/*    * Aggiornamento 03.06.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_mod),
/*       *=======================================================*/
/*       * Codice modulo                                         */
/*       *-------------------------------------------------------*/
  cod_mod                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione modulo                                    */
/*       *-------------------------------------------------------*/
  des_mod                   varchar  (80)               not null,
  
  
  
  
  
CREATE TABLE pdf_cnf (
  cod_mod VARCHAR(8) NOT NULL,           -- Codice modulo (es. 'fattura')
  des_mod VARCHAR(80) NULL,               -- Descrizione modulo (es. 'fattura di vendita')
  key_mod VARCHAR(50) NOT NULL,           -- Chiave configurazione (es. 'fnt_fml')
  val_mod TEXT NULL,                      -- Valore configurazione (stringa o JSON)
  com_mod VARCHAR(255) NULL,              -- Commento/descrizione della chiave
  PRIMARY KEY (cod_mod, key_mod)
);
  
  
  
  
  
/*       *-------------------------------------------------------*/
/*       * Tipo modulo                                           */
/*       *                                                       */
/*       *  - 'L' : Libero                                       */
/*       *  - 'T' : Tipografico                                  */
/*       *  - 'S' : Specifico non tipografico                    */
/*       *  - 'P' : Postscript incapsulato                       */
/*       *-------------------------------------------------------*/
  tip_mod                   varchar  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Larghezza modulo in 72mi di pollice                   */
/*       *-------------------------------------------------------*/
  l72_mod                   integer  (04)               not null,
/*       *-------------------------------------------------------*/
/*       * Altezza modulo in 72mi di pollice                     */
/*       *-------------------------------------------------------*/
  a72_mod                   integer  (04)               not null,
/*       *-------------------------------------------------------*/
/*       * Interlinee possibili per il modulo                    */
/*       *-------------------------------------------------------*/
  in1_mod                   tinyint  (03)               not null,
  in2_mod                   tinyint  (03)               not null,
/*       *-------------------------------------------------------*/
/*       * Classe di appartenenza                                */
/*       *                                                       */
/*       *  - 'S' : Standard                                     */
/*       *  - 'M' : Su misura                                    */
/*       *-------------------------------------------------------*/
  cla_mod                   varchar  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Sistema applicativo di appartenenza                   */
/*       *-------------------------------------------------------*/
  sap_mod                   varchar  (03)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice modulo a cui e' compatibile                    */
/*       *-------------------------------------------------------*/
  cmp_mod                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Si/no PostScript                                      */
/*       *                                                       */
/*       *  - 'S' : Si                                           */
/*       *  - 'N' : No                                           */
/*       *-------------------------------------------------------*/
  snx_psc                   varchar  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Si/no lettura facilitata                              */
/*       *                                                       */
/*       *  - 'S' : Si                                           */
/*       *  - 'N' : No                                           */
/*       *                                                       */
/*       * N.B.: Solo se di tipo 'Libero' e 'PostScript'         */
/*       *-------------------------------------------------------*/
  snx_lfc                   varchar  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Orientamento modulo                                   */
/*       *                                                       */
/*       *  - 'A' : Automatico                                   */
/*       *  - 'O' : Sempre orizzontale                           */
/*       *  - 'V' : Sempre verticale                             */
/*       *                                                       */
/*       * N.B.: Solo se di tipo 'Libero' e 'PostScript'         */
/*       *-------------------------------------------------------*/
  ori_mod                   varchar  (01)               not null