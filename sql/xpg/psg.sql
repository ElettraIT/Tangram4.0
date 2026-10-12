/*    *==========================================================*/
/*    * Tabella [psg] Personalizzazioni generali           {xpg} */
/*    *                                                          */
/*    * Aggiornamento 21.06.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_psg),
/*       *=======================================================*/
/*       * Codice personalizzazione                              */
/*       *                                                       */
/*       * N.B. : campo attualmente forzato a spazi              */
/*       *-------------------------------------------------------*/
  cod_psg                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione personalizzazione                         */
/*       *                                                       */
/*       * N.B. : campo attualmente forzato a spazi              */
/*       *-------------------------------------------------------*/
  des_psg                   varchar  (80)               not null,
/*       *-------------------------------------------------------*/
/*       * Controllo passwords (S/N)                             */
/*       *                                                       */
/*       * - Su codici azienda                                   */
/*       * - Su codici utente                                    */
/*       * - Su codici comando                                   */
/*       * - Su codici terminale                                 */
/*       * - Su codici stampante                                 */
/*       *-------------------------------------------------------*/
  pwd_azi                   varchar  (01)               not null,
  pwd_ute                   varchar  (01)               not null,
  pwd_cmd                   varchar  (01)               not null,
  pwd_ter                   varchar  (01)               not null,
  pwd_stp                   varchar  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Controllo riservatezza (S/N)                          */
/*       *                                                       */
/*       * - Su tasks                                            */
/*       * - Su menu'                                            */
/*       * - Su procedure                                        */
/*       * - Su gerarchia comando                                */
/*       * - Su tipo utente                                      */
/*       * - Su livello di protezione                            */
/*       *-------------------------------------------------------*/
  rsv_tsk                   varchar  (01)               not null,
  rsv_mnu                   varchar  (01)               not null,
  rsv_prc                   varchar  (01)               not null,
  rsv_ide                   varchar  (01)               not null,
  rsv_tut                   varchar  (01)               not null,
  rsv_lip                   varchar  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * User system log                                       */
/*       *-------------------------------------------------------*/
  rsv_usl                   varchar  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Si/No visualizzazione del codice azienda              */
/*       *-------------------------------------------------------*/
  snx_azi                   varchar  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Template per il lancio dello spooler di stampa,       */
/*       * valido per le stampanti definite con canale di stampa */
/*       * 'spool'                                               */
/*       *-------------------------------------------------------*/
  tem_spl                   varchar  (80)               not null,
/*       *-------------------------------------------------------*/
/*       * Template per il blocco dello spooler di stampa,       */
/*       * valido per le stampanti definite con canale di stampa */
/*       * 'spool'                                               */
/*       *-------------------------------------------------------*/
  tem_bsp                   varchar  (80)               not null,
/*       *-------------------------------------------------------*/
/*       * Correttivo per la data di sistema                     */
/*       *-------------------------------------------------------*/
  scg_cdt                   integer  (03)               not null,
/*       *-------------------------------------------------------*/
/*       * Correttivo per l'ora di sistema                       */
/*       *-------------------------------------------------------*/
  scg_chh                   integer  (03)               not null,
/*       *-------------------------------------------------------*/
/*       * Mail server aziendale                                 */
/*       *-------------------------------------------------------*/
  msa_isp                   varchar  (80)               not null,
/*       *-------------------------------------------------------*/
/*       * Indirizzo e-mail 'postmaster'                         */
/*       *-------------------------------------------------------*/
  eml_psm                   varchar  (80)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice utente 'postmaster'                            */
/*       *-------------------------------------------------------*/
  ute_psm                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Numero giorni di validita' password                   */
/*       *-------------------------------------------------------*/
  ngg_val                   integer  (05)               not null,
/*       *-------------------------------------------------------*/
/*       * Tipo azione da intraprendere alla scadenza            */
/*       *                                                       */
/*       * - 01 : Nessuna azione                                 */
/*       * - 02 : Blocco utente                                  */
/*       * - 03 : Avviso all'amministratore                      */
/*       * - 04 : Avviso all'utente                              */
/*       *                                                       */
/*       * N.B. : attualmente implementati '01' e '02'           */
/*       *-------------------------------------------------------*/
  tip_asp                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Numero massimo di tentativi errati consentiti         */
/*       *-------------------------------------------------------*/
  max_err                   integer  (03)               not null,
/*       *-------------------------------------------------------*/
/*       * Tipo azione da intraprendere al superamento tentativi */
/*       *                                                       */
/*       * - 01 : Nessuna azione                                 */
/*       * - 02 : Blocco utente                                  */
/*       * - 03 : Avviso all'amministratore                      */
/*       * - 04 : Avviso all'utente                              */
/*       *                                                       */
/*       * N.B. : attualmente implementati '01' e '02'           */
/*       *-------------------------------------------------------*/
  tip_ast                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice utente Amministratore principale               */
/*       *-------------------------------------------------------*/
  ute_amm                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice utente Amministratore alternativo              */
/*       *-------------------------------------------------------*/
  alt_amm                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Password per la protezione stampe interne PDF         */
/*       *-------------------------------------------------------*/
  pdf_psi                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Password per la protezione documenti PDF              */
/*       *-------------------------------------------------------*/
  pdf_pde                   varchar  (08)               not null
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  