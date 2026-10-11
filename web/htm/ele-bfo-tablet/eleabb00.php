<?php
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);
?>

<!DOCTYPE html>
<html>
<!-- *=================================* -->
<!-- * wip - Tutti i diritti riservati * -->
<!-- * Aggiornamento : 14/03/2025      * -->
<!-- * Azienda       : ele             * -->
<!-- *=================================* -->
<head>
  <!-- *=================================* -->
  <!-- * Controllo CACHE                 * -->
  <!-- *=================================* -->
  <meta http-equiv='cache-control' content='no-cache'>
  <meta http-equiv='expires' content='0'>
  <meta http-equiv='pragma' content='no-cache'>
  <!-- *=================================* -->
  <!-- * Titolo                          * -->
  <!-- *=================================* -->
  <title>{ E L E T T R A }</title>
  <link rel='icon' type='image/x-icon' href='../icons/favicon.icon'/>
  <!-- *=================================* -->
  <!-- * Stile                           * -->
  <!-- *=================================* -->
  <link rel="stylesheet" type="text/css" href="../css_ele/ele.css"/>
  <!-- *=================================* -->
  <!-- * Javascript                      * -->
  <!-- *=================================* -->
  <script src='../jsc_ele/jquery.min.js'></script>
  <script src='../jsc_ele/jquery.redirect.js'></script>
  
</head>
<body>
  <!-- *=================================* -->
  <!-- * Separatore                      * -->
  <!-- *=================================* -->
  <hr size="3">
  <!-- *=================================* -->
  <!-- * Logo                            * -->
  <!-- *=================================* -->
  <center>
    <img src='../icons/logo-elettra.png' width='200'>
  </center>
  <hr size="3">
  
  <br><br>
  
  <!-- *=================================* -->
  <!-- * Bottoni Centrali                * -->
  <!-- *=================================* -->
  <center>
    <div class="button-container">
      <!-- Bottone per "Controllo Colli" -->
      <div class="button-wrapper" onclick="location.href='./eleabb01.php'">
        <div class="button-text"><h1>Controllo Colli</h1></div>
        <div class="controllo-colli"></div>
      </div>
      <!-- Bottone per "Messa a Scaffale" -->
      <div class="button-wrapper" onclick="location.href='./eleabb02.php'">
        <div class="button-text"><h1>Messa a Scaffale</h1></div>
        <div class="messa-scaffale"></div>
      </div>
    </div>
  </center>
  
  <br><br>
  
  <!-- *=================================* -->
  <!-- * Separatore                      * -->
  <!-- *=================================* -->
  <hr size="3">
  <!-- *=================================* -->
  <!-- * Rientro                         * -->
  <!-- *=================================* -->

  <div id="back" name="back">
            <a href="../ele.html">
                <img src="../icons/next-blue-1.png" width="50">
            </a>
  <!-- *=================================* -->
  <!-- * Copyright                       * -->
  <!-- *=================================* -->
  <center>
    <div id="copy">
      <address>
        <font size="3">&copy; wip - TANGRAM</font>
      </address>
    </div>
  </center>
</body>
</html>
