<?php
require 'abb_cla.php';
/*====================================================================*/
/* Controllo della richiesta HTTP e dei parametri                     */
/*--------------------------------------------------------------------*/
if ($_SERVER["REQUEST_METHOD"] === "POST" && isset($_POST["prodotti"]) && is_array($_POST["prodotti"])) 
{
    /*----------------------------------------------------------------*/
    /* Creiamo un'istanza della classe                                */
    /*----------------------------------------------------------------*/
    $db = new abb_cla();
    /*----------------------------------------------------------------*/
    /* Chiamiamo la funzione                                          */
    /*----------------------------------------------------------------*/
    $result = $db->insertLotti($_POST["prodotti"]); 
    /*----------------------------------------------------------------*/
    /* Restituiamo il risulta                                         */
    /*----------------------------------------------------------------*/
    echo json_encode($result);
} else {
    echo json_encode(array("status" => "error", "message" => "Richiesta non valida"));
}
?>
