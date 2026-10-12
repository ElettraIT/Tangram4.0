/*    *==========================================================*/
/*    * Tabella [fsp] Filtri di selezione prodotti         {dcp} */
/*    *                                                          */
/*    * Aggiornamento 27.10.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_fsp),
/*       *=======================================================*/
/*       * Codice filtro                                         */
/*       *-------------------------------------------------------*/
  cod_fsp                   bigint (09)                 not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione filtro                                    */
/*       *-------------------------------------------------------*/
  des_fsp                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Mnemonico filtro                                      */
/*       *-------------------------------------------------------*/
  mne_fsp                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo ordinamento, selezione                           */
/*       *                                                       */
/*       *  - '01' : Classe, gruppo, sottogr., codice prodotto   */
/*       *  - '02' : Classe, gruppo, sottogr., desc. prodotto    */
/*       *  - '03' : Codice prodotto                             */
/*       *  - '04' : Descrizione prodotto                        */
/*       *                                                       */
/*       *  - '89' : Selezione da codice catalogo                */
/*       *                                                       */
/*       *  - '99' : Selezione casuale prodotti                  */
/*       *-------------------------------------------------------*/
  tip_ord                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Classe merceologica min e max                         */
/*       *-------------------------------------------------------*/
  cla_min                   integer (05)                not null,
  cla_max                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Gruppo merceologico min e max                         */
/*       *-------------------------------------------------------*/
  gru_min                   integer (05)                not null,
  gru_max                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottogruppo merceologico min e max                    */
/*       *-------------------------------------------------------*/
  sgr_min                   integer (05)                not null,
  sgr_max                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto alfanumerico min e max                */
/*       *-------------------------------------------------------*/
  alf_min                   varchar (20)                not null,
  alf_max                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione prodotto, uppercase, min e max            */
/*       *-------------------------------------------------------*/
  des_min                   varchar (80)                not null,
  des_max                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Sinonimo min - max                                    */
/*       *-------------------------------------------------------*/
  syn_min                   varchar (20)                not null,
  syn_max                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Barcode min - max                                     */
/*       *-------------------------------------------------------*/
  klb_min                   varchar (20)                not null,
  klb_max                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo prodotto                                         */
/*       *                                                       */
/*       *  - 01 : Merce                                         */
/*       *  - 02 : Servizio                                      */
/*       *  - 03 : Imballo                                       */
/*       *  - 09 : Extra attivita' aziendale                     */
/*       *-------------------------------------------------------*/
  tip_pro                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice Iva per la vendita                             */
/*       *-------------------------------------------------------*/
  cod_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Conto di contabilita' generale usato come contropar-  */
/*       * tita per le vendite del prodotto                      */
/*       *-------------------------------------------------------*/
  ctp_ven                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura per la vendita                       */
/*       *-------------------------------------------------------*/
  umi_ven                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codici statistici prodotto                            */
/*       *-------------------------------------------------------*/
  cod_s01                   integer (05)                not null,
  cod_s02                   integer (05)                not null,
  cod_s03                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice fornitore preferenziale                        */
/*       *-------------------------------------------------------*/
  cod_fnt                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Singole definizioni status prodotti                   */
/*       *                                                       */
/*       *  - Spaces : No                                        */
/*       *  - X      : Si                                        */
/*       *-------------------------------------------------------*/
  sta_nor                   varchar (1)                 not null,
  sta_esa                   varchar (1)                 not null,
  sta_sos                   varchar (1)                 not null,
  sta_ces                   varchar (1)                 not null,
  sta_cms                   varchar (1)                 not null,
  sta_obs                   varchar (1)                 not null,
  sta_oms                   varchar (1)                 not null,
/*       *-------------------------------------------------------*/
/*       * Specifica libera per il prodotto                      */
/*       *-------------------------------------------------------*/
  spc_lib                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Data inizio commercializzazione prodotto o data codi- */
/*       * fica min e max                                        */
/*       *-------------------------------------------------------*/
  icm_min                   date                        not null,
  icm_max                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice casa produttrice                               */
/*       *-------------------------------------------------------*/
  cod_pdt                   integer (07)                not null