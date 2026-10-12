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
  PRIMARY KEY                                          (tip_mag,
                                                        num_mag,
                                                        cod_dcf,
                                                        fda_pif,
                                                        prg_cst),
/*       *=======================================================*/
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
/*       * Codice fornitore                                      */
/*       *-------------------------------------------------------*/
  cod_dcf                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Formato di acquisizione presso il fornitore           */
/*       *-------------------------------------------------------*/
  fda_pif                   varchar (20)                not null,
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
