<?php
require 'abb_cla.php';

/*====================================================================*/
/* Controllo della richiesta HTTP e dei parametri                     */
/*--------------------------------------------------------------------*/
if ($_SERVER["REQUEST_METHOD"] === "POST" &&
    isset($_POST["num_prg"]) &&
    isset($_POST["alf_pro"]) &&
    isset($_POST["ubi_qta"])) {

    $num_prg = $_POST["num_prg"];
    $alf_pro = strtoupper(trim($_POST["alf_pro"]));
    $ubi_qta = floatval($_POST["ubi_qta"]);

    /*================================================================*/
    /* Creazione dell'istanza della classe abb_cla                    */
    /*================================================================*/
    $db = new abb_cla();

    /*================================================================*/
    /* Chiamata al metodo updateLotto                                 */
    /*================================================================*/
    $result = $db->updateLotto($num_prg, $alf_pro, $ubi_qta);

    /*================================================================*/
    /* Invio della risposta JSON                                      */
    /*================================================================*/
    echo json_encode($result);
} else {
    echo json_encode(array("status" => "error", "message" => "Richiesta non valida"));
}
?>
