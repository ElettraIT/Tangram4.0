<?php
require 'abb_cla.php';

/*====================================================================*/
/* Controllo della richiesta HTTP                                     */
/*--------------------------------------------------------------------*/
if ($_SERVER["REQUEST_METHOD"] === "POST" && isset($_POST["num_prg"])) {
    $num_prg = $_POST["num_prg"]; // Numero progressivo del lotto ricevuto

    /*================================================================*/
    /* Creazione dell'istanza della classe abb_cla                    */
    /*================================================================*/
    $abb = new abb_cla();
    
    /*================================================================*/
    /* Recupero prodotti del lotto                                    */
    /*================================================================*/
    $prodotti = $abb->getProdotti($num_prg);

    /*================================================================*/
    /* Invio della risposta JSON                                      */
    /*================================================================*/
    echo json_encode(array("status" => "success", "prodotti" => $prodotti));
} else {
    echo json_encode(array("status" => "error", "message" => "Richiesta non valida"));
}
?>
