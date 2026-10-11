<?php
/*====================================================================*/
/* Modulo             : eleabb02.php                                  */
/* ------------------------------------------------------------------ */
/* Ultima revisione   : Andrea del 14/03/25                           */
/* ================================================================== */
/*                                                                    */
/* Descrizione        : Messa a scaffale                              */
/*                                                                    */
/* Funzioni principali:                                               */
/*  - Inclusione del modulo per lettura tabelle (let_tab.php)         */
/*  - Lettura dei parametri POST per mantenere lo stato               */
/*  - Creazione pagina per posizionamento a scaffale prodotti in LTRA */
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
/*  $liv_ope  : Livello Operativo                                     */
/*  $rsp_doc  : Codice responsabile selezionato                       */
/*  $rsp_des  : Descrizione (nome) del responsabile                   */
/*  $num_prg  : Numero progressivo lotto                              */
/*====================================================================*/
$liv_ope = isset($_POST['liv_ope']) ? $_POST['liv_ope'] :  0;
$rsp_doc = isset($_POST['rsp_doc']) ? $_POST['rsp_doc'] : '';
$rsp_des = isset($_POST['rsp_des']) ? $_POST['rsp_des'] : '';
$num_prg = isset($_POST['num_prg']) ? $_POST['num_prg'] : '';

?>
<!DOCTYPE html>
<html>
<head>
    <!--=============================================================-->
    <!-- Meta & Titolo                                               -->
    <!----------------------------------------------------------------->
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Messa a Scaffale {ELETTRA}</title>
    <link rel="icon" type="image/x-icon" href="../icons/favicon.ico">
    <!--=============================================================-->
    <!-- Inclusione Fogli di Stile e Script                          -->
    <!----------------------------------------------------------------->
    <link rel="stylesheet" type="text/css" href="../css_ele/jquery-ui.min.css">
    <link rel="stylesheet" type="text/css" href="../css_ele/ele.css">
    <script src="../jsc_ele/jquery.min.js"></script>
    <script src="../jsc_ele/jquery-ui.min.js"></script>
    <script src="../jsc_ele/jquery.redirect.js"></script>
    <script src="../jsc_ele/eleabb02.js"></script>
    <script src="../jsc_ele/eleipx02.js"></script>
    <script src="../jsc_ele/eleubiJ0.js"></script>
    <style>
        #listaProdotti {display: none;}
        #infoDocumento {display: none;}
        #listaLotto    {display: none;}
    </style>
</head>

<body>
    <!--=============================================================-->
    <!-- Inizio Form Principale                                      -->
    <!----------------------------------------------------------------->
    <form name="bfo_002" id="bfo_002" method="post" action="eleabb02.php">
    <?php
    /*================================================================*/
    /* Switch: Gestione dei livelli operativi                         */
    /* ---------------------------------------------------------------*/
    /* 0 : Accettazione operatore                                     */
    /* 1 : Inserimento prodotti per creazione lotto                   */
    /* 2 : Messa a scaffale dei prodotti all'interno del lotto        */
    /*================================================================*/
    switch ($liv_ope) {
        case "0": 

        /*------------------------------------------------------------*/
        /* Fase 0: Accettazione operatore                             */
        /*------------------------------------------------------------*/            
        $operatori = $db->getOperatori();
        ?>       

        <!------------------------------------------------------------->
        <!-- Tabella per la selezione dell'Operatore                 -->
        <!------------------------------------------------------------->
        <table class="tes" id="key_lv0" name="key_lv0">
            <tr>
                <!-- * Cella utente * -->
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
        /*------------------------------------------------------------*/
        /* Fase 1: Creazione lista prodotti                           */
        /*------------------------------------------------------------*/
        $lotti = $db->getLotti();
        ?>
        <!-- Tabella per i dati di base -->
        <table class="tes" id="tes" name="tes">

            <tr>
                <!-- * Lista Lotti * -->
                <!-- <td id="cel_lot" name="cel_lot" align="center">
                    <h1> Lista Lotti </h1>
                    <select name="num_lot" id="num_lot"> -->
                        <!-- <option value="0" selected>(non selezionato)</option> -->
                        <!----------------------------------------->
                        <!-- Ciclo per stampa lotti aperti       -->
                        <!----------------------------------------->
                        <!-- <?php foreach ($lotti as $lt): ?> -->
                        <!-- <?php 
                        /*-----------------------------------------*/
                        /* Estrae la parte progressiva del numero  */
                        /* lotto (escludendo i primi 3 caratteri)  */
                        /* e rimuove gli zeri iniziali             */
                        /*-----------------------------------------*/
                        // $display_value = ltrim(substr($lt['num_prg'], 3), '0'); 
                        ?> -->
                        <!-- <option value="<?php echo $lt['num_prg']; ?>">
                            <?php echo htmlspecialchars($display_value . " - " . $lt['rsp_lot']); ?>
                        </option> -->
                    <!-- <?php endforeach; ?> -->
                    <!-- </select>
                </td> -->
                <!-- Visualizzazione nome operatore -->
                <td>
                    <b><?php echo $rsp_des ? $rsp_des : "(Operatore non selezionato)"; ?></b>
                </td>
                    <!-- * Prodotto * -->
                <td id="cel_pro" name="cel_pro" align="center">
                    <h1>Prodotto</h1>
                    <input type="text" id="alf_pro" name="alf_pro" value="" 
                            style="text-transform: uppercase;" maxlength="18">
                </td>
            <!-- * Pulsante Conferma * -->
            <td id="cel_cnf" name="cel_cnf" class="td_nb">
                <button type="button" id="but_cnf" name="but_cnf">
                    <img src="../icons/check.png" width="60">
                </button>
            </td>
            <!-- * Pulsante per cancellare * -->
            <td id="cel_res" name="cel_res">
                <button type="button" id="res" name="res">
                    <img src="../icons/gomma.png" width="40">
                </button>
            </td>

            </tr>
        </table>
        <!--=====================================================-->
        <!-- Div per messaggi                                    -->
        <!--=====================================================-->
        <div id="msg" name="msg"></div>
        <hr>
        <!--=====================================================-->
        <!-- Sezione Tabella dei Risultati                       -->
        <!--=====================================================-->
        <div id="listaProdotti">
                <table class="bordotab" id="pro" name="pro" width="40%">
                    <thead>
                        <tr>
                            <th align="center"><b>Prodotto</b></th>
                            <th align="center"><b>Quantità</b></th>
                        </tr>
                    </thead>
                    <!--------------------------------------------->
                    <!-- Righe aggiunte dinamicamente con js     -->
                    <!--------------------------------------------->
                    <tbody>
                    </tbody>
                </table>
        </div>
        <input type="hidden" name="rsp_doc" id="rsp_doc" value="<?php echo $rsp_doc; ?>">

        <?php
        break;
        case "2":
        /*------------------------------------------------------------*/
        /* Fase 2: Messa a scaffale dei prodotti in lista             */
        /*------------------------------------------------------------*/  
        ?>
        <!-- Tabella per i dati di base -->
        <table class="tes" id="tes" name="tes">

            <tr>
                <!-- Visualizzazione numero Lotto -->
                <td>
                    <b><h1>Lotto</h1></b>
                    <?php
                    /*-----------------------------*/
                    /* Pulizia numero lotto        */
                    /*-----------------------------*/
                        $lotto = substr($num_prg, 3);
                        $lotto = ltrim($lotto, '0');
                        if ($lotto === "") {
                            $lotto = "0";
                        }
                        if (strlen($lotto) == 1) {
                            $lotto = str_pad($lotto, 2, '0', STR_PAD_LEFT);
                        }
                    ?>
                    <b><?php echo $num_prg ? $lotto : "(Nessun Lotto)"; ?></b>
                </td>

                <!-- Visualizzazione nome operatore -->
                <td>
                    <b><?php echo $rsp_des ? $rsp_des : "(Operatore non selezionato)"; ?></b>
                </td>
                    <!-- * Prodotto * -->
                <td id="cel_pro" name="cel_pro" align="center">
                    <h1>Prodotto</h1>
                    <input type="text" id="alf_pro" name="alf_pro" value="" 
                            style="text-transform: uppercase;" maxlength="18">
                </td>
                    <!-- * Pulsante per cancellare * -->
                <td id="cel_res" name="cel_res">
                    <button type="button" id="res" name="res">
                        <img src="../icons/gomma.png" width="70">
                    </button>
                <td id='cel_ref' name='cel_ref'>
                    <button type='button' id='but_ref' name='but_ref'>
                        <img src='../icons/refresh.png' width='40'>
                    </button>
                </td>

            </tr>
        </table>
        <hr>
        <table class='bordotab' id="pro" name="pro" align='center' width=80% >
            <tr>
                <th align='center' > <B> Quantità in LTRA </B> </th>
                <th align='center' > <B> Quantità da inserire</B> </th>
                <th align='center' > <B> Ubicazione di destinazione </B> </th>
                <th align='center' > <B> Conferma </B> </th>
            </tr>
            <tr>
                <!-- * Cella Quantità presente in ubicazione * -->
                <td id='cel_qtu' name='cel_qtu' align="center">
                </td>
                <!-- * Cella Quantità da inserire * -->
                <td id='cel_qta' name='cel_qta'>
                    <input type='number'
                           id='ubi_qta' name ='ubi_qta'
                           class='numbersOnly'
                           value='' min='0' max='999999'>
                </td>
                <!-- * Cella Ubicazione destinazione * -->
                <td id='cel_ubi' name='cel_ubi'>
                    <input type='text' id='acc_ubi' name='acc_ubi'  
                    class ='acc_ubi' value='' 
                    style='text-transform: uppercase;' maxlength='07' />
                </td>
                <!-- * Tasto conferma * -->
                <td id='cel_cnf' name='cel_cnf' class='td_nb'>
                    <button type='button' id='but_cnf' name='but_cnf' >
                    <img src='../icons/check.png' width='40'>
                    </button>
                </td>
            </tr>
        </table>
        <!--=====================================================-->
        <!-- Div per messaggi                                    -->
        <!--=====================================================-->
        <div id="msg" name="msg" ></div>
        <!--=====================================================-->
        <!-- Sezione Tabella dei Risultati                       -->
        <!--=====================================================-->
        <div id="listaLotto">
            <hr style="width:80%">
                <table class="bordotab" id="lot" name="lot" width="80%">
                    <thead>
                        <tr>
                            <th align="center"><b>Prodotto</b></th>
                            <th align="center"><b>Qta in LTRA</b></th>
                            <th align="center"><b>Ubicazione principale</b></th>
                            <th align="center"><b>Qta in ubicazione</b></th>
                            <th align="center"><b>Altre ubicazioni</b></th>
                        </tr>
                    </thead>
                    <!--------------------------------------------->
                    <!-- Righe aggiunte dinamicamente con js     -->
                    <!--------------------------------------------->
                    <tbody>
                    </tbody>
                </table>

        </div>
        <input type="hidden" name="rsp_doc" id="rsp_doc" value="<?php echo $rsp_doc; ?>">
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
    <!-- Campi hidden per il modulo                          -->
    <!--=====================================================-->
    <input type="hidden" name="num_prg" id="num_prg" value="<?php echo $num_prg; ?>">
    <input type="hidden" name="rsp_des" id="rsp_des" value="<?php echo $rsp_des; ?>">
    <input type="hidden" name="liv_ope" id="liv_ope" value="<?php echo $liv_ope; ?>">
  </form>
  <!-- *=================================* -->
  <!-- * Fine Form Gestione Ubicazione  * -->
  <!-- *=================================* -->

  <!-- *=================================* -->
  <!-- * Dialog di Conferma (modal)     * -->
  <!-- *=================================* -->
  <div id="dialog-confirm" name="dialog-confirm" title="">
    <!-- Qui verrà inserito il contenuto del dialog -->
  </div>

  <!-- *=================================* -->
  <!-- * Pulsante di Back              * -->
  <!-- *=================================* -->
  <?php if ($liv_ope == 2): ?>
  <!-- Pulsante di Back per livello operativo 2 -->
  <div id="back" name="back">
    <a href="./eleabb02.php">
      <img src="../icons/next-blue-1.png" width="50">
    </a>
  </div>
    <?php else: ?>
    <!-- Pulsante di Back per gli altri livelli -->
    <div id="back" name="back">
        <a href="./eleabb00.php">
        <img src="../icons/next-blue-1.png" width="50">
        </a>
    </div>
    <?php endif; ?>

  <!-- *=================================* -->
  <!-- * Copyright                     * -->
  <!-- *=================================* -->
  <div id="copy" name="copy">
    <address>
      <font size="3">&copy; wip - TANGRAM</font>
    </address>
  </div>
</body>
</html>
