/*    *==========================================================*/
/*    * Tabella [hbc] Righe appoggio spedizioni            {agb} */
/*    *                                                          */
/*    * Estensione a righe per scansioni                         */
/*    *                                                          */
/*    * Aggiornamento 16.12.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt,
                                                        num_pro,
                                                        num_spl),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice numerico prodotto                              */
/*       *                                                       */
/*       * N.B.: se il codice prodotto e' a zero, il record      */
/*       *       contiene il percorso dell'immagine o del docu-  */
/*       *       mento PDF associato al protocollo               */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  num_pro                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo split                              */
/*       *-------------------------------------------------------*/
  num_spl                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' relativa alla sub-riga                      */
/*       *-------------------------------------------------------*/
  qta_spl                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Informazioni dedotte da barcode produttore            */
/*       *-------------------------------------------------------*/
  dat_bc1                   varchar (80)                not null,
  dat_bc2                   varchar (80)                not null,
  dat_bc3                   varchar (80)                not null,
  dat_bc4                   varchar (80)                not null,
  dat_bc5                   varchar (80)                not null,
  dat_bc6                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Annotazioni relativa alla sub-riga                    */
/*       *-------------------------------------------------------*/
  ann_spl                   varchar (256)               not null,
/*       *-------------------------------------------------------*/
/*       * Percorso immagine eventuale                           */
/*       *-------------------------------------------------------*/
  pth_img                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla bancale                                         */
/*       *-------------------------------------------------------*/
  sgl_bnc                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo ordine di spedizione                */
/*       *                                                       */
/*       * N.B.: solo in caso di 'abbinamento' da BFO            */
/*       *-------------------------------------------------------*/
  prt_ods                   bigint  (11)                not null
