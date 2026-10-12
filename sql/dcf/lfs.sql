/*    *==========================================================*/
/*    * Tabella [aap] Dati di acquisto prodotti            {dcf} */
/*    *                                                          */
/*    * Estensione ad anagrafica : castelletto prezzi            */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_rec,
                                                        cod_dcf,
                                                        sgl_vlt,
                                                        tip_mag,
                                                        num_mag,
                                                        fda_pif,
                                                        dva_fin,
                                                        prg_cst),
/*       *=======================================================*/
/*       * Tipo record                                           */
/*       *                                                       */
/*       * - 01 : Listini di acquisto prodotti per fornitore     */
/*       *-------------------------------------------------------*/
  tip_rec                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice fornitore                                      */
/*       *-------------------------------------------------------*/
  cod_dcf                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla valuta per il prezzo                            */
/*       *-------------------------------------------------------*/
  sgl_vlt                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo codice di magazzino                              */
/*       *                                                       */
/*       * - 01 : Prodotti di vendita                            */
/*       * - 03 : Materie prime                                  */
/*       * - 04 : Materiali vari                                 */
/*       *-------------------------------------------------------*/
  tip_mag                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto numerico                              */
/*       *-------------------------------------------------------*/
  num_mag                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Formato di acquisizione presso il fornitore           */
/*       *-------------------------------------------------------*/
  fda_pif                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di validita' finale                              */
/*       *                                                       */
/*       * N.B.: Data di storicizzazione del listino, ovvero     */
/*       *       data fino alla quale valgono prezzi e sconti    */
/*       *-------------------------------------------------------*/
  dva_fin                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo castelletto                               */
/*       *-------------------------------------------------------*/
  prg_cst                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' di riferimento per prezzi e sconti, espres- */
/*       * sa in conformita' alla unita' di misura del fornitore */
/*       *-------------------------------------------------------*/
  qta_pes                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Prezzo unitario fino al non superamento della quan-   */
/*       * tita', riferito alla quantita' unitaria in termini di */
/*       * unita' di misura del fornitore, in valuta             */
/*       *-------------------------------------------------------*/
  prz_pes                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice categoria sconto in riga applicata fino al non */
/*       * superamento della quantita'                           */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  csr_pes                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % di sconto in riga 1..5 applicate fino al non        */
/*       * superamento della quantita'                           */
/*       *-------------------------------------------------------*/
  psr_pe1                   decimal (05,02)             not null,
  psr_pe2                   decimal (05,02)             not null,
  psr_pe3                   decimal (05,02)             not null,
  psr_pe4                   decimal (05,02)             not null,
  psr_pe5                   decimal (05,02)             not null
