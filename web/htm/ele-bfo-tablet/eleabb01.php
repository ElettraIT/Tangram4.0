<?php
/*====================================================================*/
/* Modulo             : eleabb01.php                                  */
/* ------------------------------------------------------------------ */
/* Versione originale : 001 del 21/02/25                              */
/* Ultima revisione   : Andrea del 07/03/25                           */
/* ================================================================== */
/*                                                                    */
/* Descrizione        : Gestione carichi BFO                          */
/*                      Implementazione dei livelli operativi         */
/*                      (0: Accettazione operatore, 1: Inserimento    */
/*                          documento e poplamento tabella)           */
/*                                                                    */
/* Funzioni principali:                                               */
/*  - Inclusione del modulo per lettura tabelle (let_tab.php)         */
/*  - Lettura dei parametri POST per mantenere lo stato               */
/*  - Switch per la visualizzazione delle diverse fasi                */
/*                                                                    */
/*====================================================================*/

/*====================================================================*/
/* Inclusione classe per moduli ABB                                   */
/*====================================================================*/
include 'abb_cla.php';
$db = new abb_cla();

/*====================================================================*/
/* Lettura Parametri POST                                             */
/* -------------------------------------------------------------------*/
/*  $liv_ope  : Livello operativo                                     */
/*  $rsp_doc  : Codice responsabile selezionato                       */
/*  $rsp_des  : Descrizione (nome) del responsabile                   */
/*  $num_doc  : Numero documento inserito                             */
/*====================================================================*/
$liv_ope = isset($_POST['liv_ope']) ? $_POST['liv_ope'] : 0;
$rsp_doc = isset($_POST['rsp_doc']) ? $_POST['rsp_doc'] : '';
$rsp_des = isset($_POST['rsp_des']) ? $_POST['rsp_des'] : '';
$num_doc = isset($_POST['num_doc']) ? $_POST['num_doc'] : '';

?>
<!DOCTYPE html>
<html lang="en">
<head>
    <!--=============================================================-->
    <!-- Meta & Titolo                                               -->
    <!----------------------------------------------------------------->
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Controllo Colli {ELETTRA}</title>
    <link rel="icon" type="image/x-icon" href="../icons/favicon.ico">
    <!--=============================================================-->
    <!-- Inclusione Fogli di Stile e Script                          -->
    <!----------------------------------------------------------------->
    <link rel="stylesheet" type="text/css" href="../css_ele/jquery-ui.min.css">
    <link rel="stylesheet" type="text/css" href="../css_ele/ele.css">
    <script src="../jsc_ele/jquery.min.js"></script>
    <script src="../jsc_ele/jquery-ui.min.js"></script>
    <script src="../jsc_ele/jquery.redirect.js"></script>
    <script src="../jsc_ele/eleabb01.js"></script>
    <script src="../jsc_ele/eleipx02.js"></script>
    <script src="../jsc_ele/eleubiJ0.js"></script>
    <style>
        #dataTableContainer {display: none;}
        #infoDocumento {display: none;}

    </style>
</head>
<body>
    <!--=============================================================-->
    <!-- Inizio Form Principale                                      -->
    <!----------------------------------------------------------------->
    <form name="bfo_001" id="bfo_001" method="post" action="eleabb01.php">
    
    <?php
    /*================================================================*/
    /* Switch: Gestione dei livelli operativi                         */
    /* ---------------------------------------------------------------*/
    /* 0 : Accettazione operatore                                     */
    /* 1 : Inserimento numero documento e visualizzazione tabella     */
    /*================================================================*/
    switch ($liv_ope) {
        case "0": 
            /*--------------------------------------------------------*/
            /* Fase 0: Accettazione operatore                         */
            /*--------------------------------------------------------*/
            $operatori = $db->getOperatori();
            ?>                
                <!----------------------------------------------------->
                <!-- Campo hidden per la descrizione dell'operatore  -->
                <!----------------------------------------------------->
                <input type="hidden" name="rsp_des" id="rsp_des" value="">
                <!----------------------------------------------------->
                <!-- Tabella per la selezione dell'Operatore         -->
                <!----------------------------------------------------->
                <table class="tes" id="key_lv0" name="key_lv0">
                <tr>
                    <td id="cel_ute" name="cel_ute" align="center">
                        <h1> Operatore </h1>
                        <select name="rsp_doc" id="rsp_doc">
                            <option value="0" selected>(non selezionato)</option>
                            <!----------------------------------------->
                            <!-- Ciclo per la stampa degli operatori -->
                            <!----------------------------------------->
                            <?php foreach ($operatori as $op): ?>
                                <option value="<?php echo $op['cod_rsp']; ?>">
                                    <?php echo htmlspecialchars($op['des_rsp']); ?>
                                </option>
                            <?php endforeach; ?>
                        </select>
                    </td>
                </tr>
                </table>
            <?php
            break;            
        case "1":
            /*--------------------------------------------------------*/
            /* Fase 1: Inserimento documento e visualizz. tabella     */
            /*--------------------------------------------------------*/
            ?>
                <!-- Tabella per i dati di base -->
                <table class="tes" id="tes" name="tes">
                    <tr>
                        <!-- Visualizzazione immagine ABB -->
                        <td>
                            <b>
                                <img src="../icons/logo-abb.png" width="70">
                            </b>
                        </td>
                        <!-- Visualizzazione nome operatore -->
                        <td>
                            <b><?php echo $rsp_des ? $rsp_des : "(Operatore non selezionato)"; ?></b>
                        </td>
                        <!-- Pulsante per eventuale reset -->
                        <td id="cel_res" name="cel_res" class="td_nb">
                            <button type="button" id="res" name="res">
                                <img src="../icons/gomma.png" width="70">
                            </button>
                        </td>
                        <!-- Campo per inserimento numero documento -->
                        <td id="cel_num" name="cel_num" align="center">
                            <h1>Identificativo documento</h1>
                            <input type="text" id="num_doc" name="num_doc" 
                                    value="<?php echo $num_doc; ?>" 
                                    style="text-transform: uppercase;" maxlength="18">
                        </td>
                        <!-- Pulsante per info documento -->
                        <td id="cel_inf" name="cel_inf" class="td_nb">
                            <button type="button" id="inf" name="inf">
                                <img src="../icons/info.png" width="70">
                            </button>
                        </td>
                        <!-- Campo per eventuale inserimento prodotto -->
                        <td id="cel_pro" name="cel_pro" align="center">
                            <h1>Prodotto</h1>
                            <input type="text" id="alf_pro" name="alf_pro" value=""
                                    style="text-transform: uppercase;" maxlength="18">
                        </td>
                        <!-- Pulsante per stampa o altre azioni -->
                        <td id="cel_stp" name="cel_stp" class="td_nb">
                            <button type="button" id="stp" name="stp">
                                <img src="../icons/printer.png" width="70">
                            </button>
                        </td>
                            <!-- Visualizzazione immagine ABB -->
                            <td>
                            <b>
                                <img src="../icons/logo-abb.png" width="70">
                            </b>
                        </td>

                    </tr>
                </table>
            <!--=====================================================-->
            <!-- Tabella per informazioni documento                  -->
            <!--=====================================================-->
            <div id="infoDocumento">
                <table class="bordotab" id="rif" name="rif" width="60%">
                    <!------------------------------------------------->
                    <!-- Righe aggiunte dinamicamente con js         -->
                    <!------------------------------------------------->
                    <tbody>
                    </tbody>
                </table>
            </div>
            <!--=====================================================-->
            <!-- Sezione Tabella dei Risultati                       -->
            <!--=====================================================-->
            <div id="dataTableContainer">
                <hr style="width:100%">
                    <table class="bordotab" id="rig" name="rig" width="100%">
                        <thead>
                            <tr>
                                <th align="center"><b>NR</b></th>
                                <th align="center"><b>Codice</b></th>
                                <th align="center"><b>Descrizione</b></th>
                                <th align="center"><b>C.Produttore</b></th>
                                <th align="center"><b>CTN</b></th>
                                <th align="center"><b>Ordinato</b></th>
                                <th align="center"><b>Verificato</b></th>
                                <th align="center"><b>V</b></th>
                                <th align="center"><b>Ubicazione</b></th>
                                <th align="center"><b>Giacenza</b></th>
                                <th align="center"><b>NC</b></th>
                            </tr>
                        </thead>
                        <!--------------------------------------------->
                        <!-- Righe aggiunte dinamicamente con js     -->
                        <!--------------------------------------------->
                        <tbody>
                        </tbody>
                    </table>
            </div>
            <!--=====================================================-->
            <!-- Div per messaggi                                    -->
            <!--=====================================================-->
            <div id="msg" name="msg" class="msg_red"></div>
            <!--=====================================================-->
            <!-- Fine Sezione Tabella dei Risultati                  -->
            <!--=====================================================-->
            <input type="hidden" name="rsp_doc" id="rsp_doc" value="<?php echo $rsp_doc; ?>">
            <input type="hidden" name="rsp_des" id="rsp_des" value="<?php echo $rsp_des; ?>">
            <input type="hidden" name="prt_bfo" id="prt_bfo" value="<?php echo $num_doc; ?>">
            <input type="hidden" name="ubi_vrt" id="ubi_vrt" value="<?php echo $ubi_vrt; ?>">
            <?php
            break;
        
        default:
            /*--------------------------------------------------------*/
            /* Caso Default: Livello operativo non riconosciuto       */
            /*--------------------------------------------------------*/
            ?>
            <p>Livello operativo non riconosciuto.</p>
            <?php
            break;
        }
    ?>
            <!--=====================================================-->
            <!-- Campo hidden livello operativo                      -->
            <!--=====================================================-->
        <input type="hidden" name="liv_ope" id="liv_ope" value="<?php echo $liv_ope; ?>">
    </form>
    
    <hr style="width:100%">
    <center>
        <div id="back" name="back">
            <a href="./eleabb00.php">
                <img src="../icons/next-blue-1.png" width="50">
            </a>
        </div>
        <div id="copy" name="copy">
            <address>
                <font size="3">&copy; wip - TANGRAM</font>
            </address>
        </div>
        <div id="dialog" name="dialog"></div>
    </center>

</body>
</html>
