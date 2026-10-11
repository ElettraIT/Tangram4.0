<?php
header('Content-Type: application/json');

// Dati in array associativo
$data = array(
    "2022" => array(10, 20, 300, 40, 50, 60, 70, 80, 90, 100, 110, 120),
    "2023" => array(15, 25, 250, 45, 55, 65, 75, 85, 95, 105, 115, 125),
    "2024" => array(20, 30, 405, 50, 60, 70, 80, 90, 100, 110, 120, 130)
);

// Funzione per convertire l'array in JSON manualmente
function array_to_json($array) {
    $json = "{";
    $firstYear = true;
    
    foreach ($array as $year => $values) {
        if (!$firstYear) {
            $json .= ",";
        }
        $json .= "\"$year\":[" . implode(",", $values) . "]";
        $firstYear = false;
    }
    
    $json .= "}";
    return $json;
}

// Stampiamo il JSON costruito manualmente
echo array_to_json($data);
?>
