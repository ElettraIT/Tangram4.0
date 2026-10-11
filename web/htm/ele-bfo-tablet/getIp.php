<?php
// Imposta l'header per indicare che la risposta è in formato testo semplice
header('Content-Type: text/plain');

// Determina l'indirizzo IP del client
if (!empty($_SERVER['HTTP_CLIENT_IP'])) {
    $ip = $_SERVER['HTTP_CLIENT_IP'];
} elseif (!empty($_SERVER['HTTP_X_FORWARDED_FOR'])) {
    // In caso di proxy, HTTP_X_FORWARDED_FOR può contenere più indirizzi separati da virgola.
    $ipList = explode(',', $_SERVER['HTTP_X_FORWARDED_FOR']);
    $ip = trim($ipList[0]);
} else {
    $ip = $_SERVER['REMOTE_ADDR'];
}

// Restituisce l'indirizzo IP come stringa
echo $ip;
?>
