<?php
error_reporting(E_ALL);
ini_set('display_errors', 1);
require 'let_tab.php';

if ($_SERVER["REQUEST_METHOD"] === "POST" && isset($_POST["prodotti"]) && is_array($_POST["prodotti"])) {
    $pdo = connectDatabase();
    $prodotti = $_POST["prodotti"]; // Ora riceviamo direttamente un array

    if (empty($prodotti)) {
        echo json_encode(array("status" => "error", "message" => "Nessun prodotto ricevuto"));
        exit;
    }

    try {
        $pdo->beginTransaction(); // Avvio della transazione

        // Calcolo del secolo e anno corrente
        $anno_corrente = date("Y"); // Es. 2025
        $secoloIntero = floor($anno_corrente / 100) + 1; // Es. 21esimo secolo
        $secolo = substr($secoloIntero, -1);
        $anno = substr($anno_corrente, -2); // Prende le ultime due cifre, es. '25'
        $saa_num = $secolo . $anno; // Es. 125 per il 2025

        // Controllo se esiste già una riga con cod_num = 'abblot' e il secolo/anno corretto
        $sqlCheck = "SELECT prt_num FROM num WHERE cod_num = 'abblot' AND saa_num = :saa_num";
        $stmtCheck = $pdo->prepare($sqlCheck);
        $stmtCheck->bindParam(':saa_num', $saa_num, PDO::PARAM_INT);
        $stmtCheck->execute();
        $row = $stmtCheck->fetch(PDO::FETCH_ASSOC);

        if ($row) {
            $prt_num = $row["prt_num"] + 1; // Usa il numero progressivo esistente
        } else {
            // Se non esiste, creiamo una nuova riga in `num`
            $prt_num = ($saa_num . "00000001"); // Es. 12500000000 per il 2025

            $sqlInsertNum = "INSERT INTO num (cod_num, saa_num, prt_num) VALUES ('abblot', :saa_num, :prt_num)";
            $stmtInsertNum = $pdo->prepare($sqlInsertNum);
            $stmtInsertNum->bindParam(':saa_num', $saa_num, PDO::PARAM_INT);
            $stmtInsertNum->bindParam(':prt_num', $prt_num, PDO::PARAM_INT);
            $stmtInsertNum->execute();
        }

        // Inseriamo i prodotti in `abb_lot`
        $sqlInsertLotti = "INSERT INTO abb_lot (num_prg, alf_pro, flg_lot) VALUES (:id, :alf_pro, NULL)";
        $stmtInsertLotti = $pdo->prepare($sqlInsertLotti);

        foreach ($prodotti as $prodotto) {
            $stmtInsertLotti->bindParam(':id', $prt_num, PDO::PARAM_INT);
            $stmtInsertLotti->bindParam(':alf_pro', $prodotto['alf_pro'], PDO::PARAM_STR);
            $stmtInsertLotti->execute();
        }

        // Aggiorna il `prt_num` in `num`
        $sqlUpdateNum = "UPDATE num SET prt_num = :prt_num WHERE cod_num = 'abblot' AND saa_num = :saa_num";
        $stmtUpdateNum = $pdo->prepare($sqlUpdateNum);
        $stmtUpdateNum->bindParam(':prt_num', $prt_num, PDO::PARAM_INT);
        $stmtUpdateNum->bindParam(':saa_num', $saa_num, PDO::PARAM_INT);
        $stmtUpdateNum->execute();

        $pdo->commit(); // Conferma la transazione

        echo json_encode(array("status" => "success", "message" => "Prodotti inseriti con successo"));
    } catch (PDOException $e) {
        $pdo->rollBack();
        echo json_encode(array("status" => "error", "message" => "Errore nell'inserimento: " . $e->getMessage()));
    }
} else {
    echo json_encode(array("status" => "error", "message" => "Richiesta non valida"));
}

?>
