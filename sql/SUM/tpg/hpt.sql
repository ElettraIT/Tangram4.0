/*    *==========================================================*/
/*    * Tabella [hpt] Preventivi : testate                 {tpg} */
/*    *                                                          */
/*    * Aggiornamento 26.01.2006                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di registrazione                                 */
/*       *-------------------------------------------------------*/
  dat_reg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo commessa : s.aa.nnnnnn              */
/*       *-------------------------------------------------------*/
  prt_cml                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ns. dipendenza                                 */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice tipo preventivo                                */
/*       *                                                       */
/*       * N.B. : Questo campo attualmente non e' gestito        */
/*       *-------------------------------------------------------*/
  cod_tpr                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo preventivo                                       */
/*       *                                                       */
/*       * - 01 : Lavorazione a cliente                          */
/*       *-------------------------------------------------------*/
  tip_prv                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data chiusura preventivo                              */
/*       *-------------------------------------------------------*/
  dat_chi                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data consegna prevista                                */
/*       *-------------------------------------------------------*/
  dat_cop                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data documento di riferimento                         */
/*       *                                                       */
/*       * N.B. : Questo campo attualmente e' gestito come data  */
/*       *        aggiornamento                                  */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento di riferimento                       */
/*       *                                                       */
/*       * N.B. : Questo campo attualmente non e' gestito        */
/*       *-------------------------------------------------------*/
  num_doc                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice cliente                                        */
/*       *-------------------------------------------------------*/
  cod_cli                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza cliente                             */
/*       *-------------------------------------------------------*/
  dpz_cli                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Forma di pagamento per il preventivo                  */
/*       *-------------------------------------------------------*/
  cod_fop                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice agente                                         */
/*       *-------------------------------------------------------*/
  cod_age                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice destinatario per spedizione                    */
/*       *                                                       */
/*       * N.B. : Questo campo attualmente non e' gestito        */
/*       *-------------------------------------------------------*/
  cod_dst                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza destinatario per spedizione         */
/*       *                                                       */
/*       * N.B. : Questo campo attualmente non e' gestito        */
/*       *-------------------------------------------------------*/
  dpz_dst                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo preventivo di riferimento           */
/*       *-------------------------------------------------------*/
  prt_pdr                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice responsabile del preventivo                    */
/*       *-------------------------------------------------------*/
  cod_rdp                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Nominativo per firma preventivo                       */
/*       *-------------------------------------------------------*/
  fir_map                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Data richiesta cliente                                */
/*       *-------------------------------------------------------*/
  dat_rcl                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero richiesta cliente                              */
/*       *-------------------------------------------------------*/
  num_rcl                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Castelletto copie                                     */
/*       *-------------------------------------------------------*/
  num_cp1                   bigint  (11)                not null,
  vmn_cp1                   bigint  (11)                not null,
/*
  num_cp2                   bigint  (11)                not null,
  vmn_cp2                   bigint  (11)                not null,
/*
  num_cp3                   bigint  (11)                not null,
  vmn_cp3                   bigint  (11)                not null,
/*
  num_cp4                   bigint  (11)                not null,
  vmn_cp4                   bigint  (11)                not null,
/*
  num_cp5                   bigint  (11)                not null,
  vmn_cp5                   bigint  (11)                not null,
/*
  num_cp6                   bigint  (11)                not null,
  vmn_cp6                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione preventivo                                */
/*       *-------------------------------------------------------*/
  des_prv                   varchar (120)               not null,
/*       *-------------------------------------------------------*/
/*       * Annotazioni per il preventivo                         */
/*       *-------------------------------------------------------*/
  ann_prv                   varchar (120)               not null,
/*       *-------------------------------------------------------*/
/*       * Nome interlocutore per il preventivo                  */
/*       *-------------------------------------------------------*/
  nom_int                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di telefono per il preventivo                  */
/*       *-------------------------------------------------------*/
  num_tel                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di Fax per il preventivo                       */
/*       *-------------------------------------------------------*/
  num_fax                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * E-mail per il preventivo                              */
/*       *-------------------------------------------------------*/
  iem_ail                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di CD per il preventivo                        */
/*       *-------------------------------------------------------*/
  num_cdr                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice modalita' di consegna per il preventivo        */
/*       *                                                       */
/*       * - 01 : Da concordare                                  */
/*       * - 02 : Solita                                         */
/*       * - 03 : Franco partenza                                */
/*       * - 03 : Franco destino                                 */
/*       *-------------------------------------------------------*/
  cod_cns                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione modalita' di consegna per il preventivo   */
/*       *                                                       */
/*       * N.B. : Questo campo attualmente non e' gestito        */
/*       *-------------------------------------------------------*/
  des_cns                   varchar (30)                not null,
/*       *-------------------------------------------------------*/
/*       * Costo copia successive                                */
/*       *-------------------------------------------------------*/
  cst_ccs                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Categoria lavoro                                      */
/*       *                                                       */
/*       * - 01 : Categoria A                                    */
/*       * - 02 : Categoria B                                    */
/*       * - 03 : Categoria C                                    */
/*       *-------------------------------------------------------*/
  cat_lav                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice modalita' di pagamento per il preventivo       */
/*       *                                                       */
/*       * - 01 : Quello in anagrafica                           */
/*       * - 02 : Ri.Ba. a 60 gg. DF FM                          */
/*       * - 03 : Ri.Ba. a 90 gg. DF FM                          */
/*       *-------------------------------------------------------*/
  mod_pag                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto                                       */
/*       *-------------------------------------------------------*/
  num_pro                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * % di provvigione agente                               */
/*       *-------------------------------------------------------*/
  ppv_age                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Sconto preventivo                                     */
/*       *-------------------------------------------------------*/
  per_scp                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice Iva                                            */
/*       *-------------------------------------------------------*/
  cod_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Versione approvata preventivo                         */
/*       *-------------------------------------------------------*/
  ver_app                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di elaborazione bloccanti                       */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       * 04 : (libero)                                         */
/*       * 05 : (libero)                                         */
/*       * 06 : (libero)                                         */
/*       * 07 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_bl1                   varchar (01)                not null,
  flg_bl2                   varchar (01)                not null,
  flg_bl3                   varchar (01)                not null,
  flg_bl4                   varchar (01)                not null,
  flg_bl5                   varchar (01)                not null,
  flg_bl6                   varchar (01)                not null,
  flg_bl7                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di elaborazione NON bloccanti                   */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_nb1                   varchar (01)                not null,
  flg_nb2                   varchar (01)                not null,
  flg_nb3                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di sottoponibilita a pulizia                     */
/*       *-------------------------------------------------------*/
  flg_pul                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null

