/*========================================*/
/* eleabb01 - Javascript - N&A            */
/*                                        */
/* Elettra : Controllo colli              */
/* -------------------------------------- */
/* Aggiornamento   : 12/03/2025           */
/*========================================*/

/*========================================*/
/* Nome del modulo form                   */
/*----------------------------------------*/
mod_frm = "#bfo_001";
/*========================================*/
/* Indirizzo per lettura IP               */
/*----------------------------------------*/
url_rsp = "./getip.php";
/*----------------------------------------*/
/* Indirizzo per espansione ubicazioni    */
/*----------------------------------------*/
url_exu = "../cgi-bin/eleodsg0";
/*----------------------------------------*/
/* Indirizzo per scrittura bfo            */
/*----------------------------------------*/
url_rc0 = "../cgi-bin/eleabbX0";
/*----------------------------------------*/
/* Indirizzo per scrittura bfo            */
/*----------------------------------------*/
url_rc1 = "../cgi-bin/eleabbX1";
/*----------------------------------------*/
/* Indirizzo per lettura documento o CTN  */
/*----------------------------------------*/
url_pr1 = "../cgi-bin/eleabbD0";
/*----------------------------------------*/
/* Indirizzo per stampa documento         */
/*----------------------------------------*/
url_stp = "../cgi-bin/elebfo1s";

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function () {
    /*====================================*/
    /* Variabili globali                  */
    /*------------------------------------*/
    /* Livello operativo                  */
    /*                                    */
    /* - '0' : Accettazione responsabile  */
    /* - '1' : Accettazione protocolli    */
    /* - '2' : Accettazione prodotti      */
    /*------------------------------------*/
    liv_ope = $("#liv_ope").val();

    /*====================================*/
    /* Deviazione in funzione del livello */
    /*------------------------------------*/
    if (liv_ope == 0 || !liv_ope) {
        /*--------------------------------*/
        /* Livello '0'                    */
        /*--------------------------------*/
        liv_ope = 0;
        liv_000();
    } else if (liv_ope == 1) {
        /*--------------------------------*/
        /* Livello '1'                    */
        /*--------------------------------*/
        liv_001();
    }
    /*----------------------------------------*/
    /* Fine controllo dei dati                */
    /*========================================*/
});

/*========================================*/
/* FUNZIONI                               */
/*========================================*/

/*========================================*/
/* Funzione di gestione livello '0'       */
/*                                        */
/* Input Operatore                        */
/*----------------------------------------*/
function liv_000() {
    /*====================================*/
    /* Test iniziale su indirizzo IP per  */
    /* individuare l'operatore            */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    $.ajax({
        type: "POST",
        url: url_rsp,
        contentType: "application/x-www-form-urlencoded",
        /*--------------------------------*/
        /* Se indirizzo determinato       */
        /*--------------------------------*/
        success: function (data) {
            /*----------------------------*/
            /* Determinazione operatore   */
            /*----------------------------*/
            var ipx_val = data.trim();
            let_ipx(ipx_val);
        },
    });

    /*====================================*/
    /* Cella di accettazione in rosso     */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    cel_css("cel_ute", "R");

    /*====================================*/
    /* Cambio Operatore                   */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    $("#rsp_doc").change(function () {
        /*--------------------------------*/
        /* Se impostato                   */
        /*--------------------------------*/
        if ($(this) != "") {
            var des = $("#rsp_doc option:selected").text();
            $("#rsp_des").val(des.trim());
            /*----------------------------*/
            /* A livello successivo       */
            /*----------------------------*/
            $("#liv_ope").val("1");
        }
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit();
    });

    /*----------------------------------------*/
    /* Fine funzione livello '0'              */
    /*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '1'       */
/*                                        */
/* Input Protocolli                       */
/*----------------------------------------*/
function liv_001() {
    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione elenco 'check'          */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal: true,
        title: "Scheda prodotto",
        width: 950,
        height: "auto",
    });
    /*====================================*/
    /* Disabilito prodotto                */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#alf_pro").prop("disabled", true);

    /*====================================*/
    /* Numero documento                   */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    num_doc = $("#num_doc").val();

    if (num_doc == "") {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select();
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({ "background-color": "#BBB" });
        cel_css("cel_num", "+");
        $("#num_doc").focus();
    }

    /*====================================*/
    /* Responsabile                       */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    rsp_doc = $("#rsp_doc").val();
    rsp_des = $("#rsp_des").val();

    /*====================================*/
    /* Highlight Numero documento         */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#num_doc").click(function () {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select();
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({ "background-color": "#BBB" });
        cel_css("cel_num", "+");
    });

    /*====================================*/
    /* Cambio Numero documento            */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#num_doc").change(function () {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $("#alf_pro").val("");
        $("#prt_bfo").val("");
        $("#msg").html("");
        /*--------------------------------*/
        /* Costruzione dati               */
        /*--------------------------------*/
        var num_doc = $("#num_doc").val();
        /*--------------------------------*/
        /* Memorizzo valore in locale     */
        /*--------------------------------*/
        sessionStorage.setItem("num_doc", num_doc);
        var dat = "ide_doc=" + num_doc;
        /*================================*/
        /* Ajax per ricevere dati da      */
        /* Tangram                        */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url: url_pr1,
            data: dat,
            dataType: "json",
            success: function (rec) {
                /*----------------------------*/
                /* Se il JSON arriva come     */
                /* array prendo il primo      */
                /* elemento                   */
                /*----------------------------*/
                var recObj = Array.isArray(rec) ? rec[0] : rec;
                /*----------------------------*/
                /* Pulisco le chiavi dell     */
                /* oggetto                    */
                /*----------------------------*/
                recObj = trimKeys(recObj);
                /*----------------------------*/
                /* Controllo lo status del    */
                /* documento                  */
                /*----------------------------*/
                if (recObj.hasOwnProperty("sta_tus")) {
                    /*------------------------*/
                    /* Pulisco il valore      */
                    /*------------------------*/
                    var status = recObj.sta_tus.trim();
                    /*------------------------*/
                    /* A seconda dello status */
                    /* stampo un messaggio    */
                    /* diverso                */
                    /*------------------------*/
                    if (status === "X") {
                        $("#dataTableContainer").hide();
                        $("#rif tbody").empty();
                        $("#msg").html("<h1>FORNITORE DIVERSO DA ABB</h1>");
                        /*--------------------*/
                        /* Focus su numero    */
                        /* documento          */
                        /*--------------------*/
                        $("#num_doc").focus();
                        $("#num_doc").select();

                        return;
                    } else if (status === "N") {
                        $("#dataTableContainer").hide();
                        $("#msg").html("<h1>DOCUMENTO NON TROVATO</h1>");
                        return;
                    }
                }
                /*----------------------------*/
                /* Abilito Prodotto           */
                /*----------------------------*/
                $("#alf_pro").prop("disabled", false);
                /*----------------------------*/
                /* Separazione delle righe    */
                /*----------------------------*/
                var rows = sep_rig(rec);
                /*----------------------------*/
                /* Costruisce le tabelle      */
                /*----------------------------*/
                fil_tab(rows);
                fil_inf(rows);
                /*----------------------------*/
                /* Disabilito campi input     */
                /* righe gia spuntate         */
                /*----------------------------*/
                dis_inp();
                /*----------------------------*/
                /* Assegnazione protocollo    */
                /*----------------------------*/
                $("#prt_bfo").val(rows[1]["num_prt"]);
                /*----------------------------*/
                /* Assegnazione ubic. virtuali*/
                /*----------------------------*/
                $("#ubi_vrt").val(rows[1]["ubi_vrt"]);
                /*----------------------------*/
                /* Mostra le tabelle nascoste */
                /*----------------------------*/
                $("#dataTableContainer").show();
                /*----------------------------*/
                /* Controllo per stampa       */
                /*----------------------------*/
                var prt_bfo = $("#prt_bfo").val();
                ctr_stp(prt_bfo)
                    .then(function (flag) {
                        /*------------------------*/
                        /* Cambio colore stampante*/
                        /*------------------------*/
                        flg_stp(flag);
                    })
                    .catch(function (error) {
                        console.error("Errore in ctr_stp:", error);
                    });
                /*----------------------------*/
                /* Colorazione ubicazioni     */
                /*----------------------------*/
                ubi_ros();
                /*----------------------------*/
                /* Controllo status chiuso    */
                /*----------------------------*/
                if (
                    recObj.hasOwnProperty("sta_tus") &&
                    recObj.sta_tus.trim() === "C"
                ) {
                    $("#msg").html("Documento chiuso, modifiche disabilitate");
                    /*--------------------*/
                    /* Disabilito i campi */
                    /* ma comunque mostro */
                    /* le righe           */
                    /*--------------------*/
                    $("#rig").find("input, checkbox").prop("disabled", true);
                }
                /*----------------------------*/
                /* Focus su codice prodotto   */
                /*----------------------------*/
                $("#alf_pro").focus();
                $("#alf_pro").select();
            },
        });
    });
    /*====================================*/
    /* Recupero il numero documento       */
    /*------------------------------------*/
    var doc_svd = sessionStorage.getItem("num_doc");
    /*------------------------------------*/
    /* Se il numero presente lo assegno   */
    /* al campo e triggero l'evento change*/
    /*------------------------------------*/
    if (doc_svd) {
        $("#num_doc").val(doc_svd).trigger("change");
        $("#alf_pro").prop("disabled", false);
    }
    /*====================================*/
    /* Highlight Codice prodotto          */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $(document).on("click", "#alf_pro", function () {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select();
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({ "background-color": "#BBB" });
        cel_css("cel_pro", "+");
    });

    /*====================================*/
    /* Bottone reset                      */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#res").click(function () {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        sessionStorage.removeItem("num_doc");
        $("#num_doc").val("");
        $("#prt_bfo").val("");
        $("#alf_pro").val("");
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit();
    });

    /*====================================*/
    /* Bottone informazioni               */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#inf").on("click", function () {
        /*--------------------------------*/
        /* Se tbody contiene righe        */
        /*--------------------------------*/
        if ($("#rif tbody").children().length > 0) {
            $("#infoDocumento").toggle();
        }
    });

    /*====================================*/
    /* Gestione del cambio del prodotto   */
    /*------------------------------------*/
    $(document).on("change", "#alf_pro", function () {
        /*--------------------------------*/
        /* Valori per chiamata            */
        /*--------------------------------*/
        var alf_pro = $(this).val().trim().toUpperCase();
        var tip_ope = "LP";
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm = {
            tip_ope: tip_ope,
            alf_pro: alf_pro,
        };
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm);
        /*================================*/
        /* Chiamata ajax per ricerca con  */
        /* Barcode                        */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url: url_rc1,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            success: function (rec) {
                /*------------------------*/
                /* Passo risultato per    */
                /* evidenziare la riga    */
                /*------------------------*/
                hig_rig(rec.trim());
            },
            error: function (xhr, status, error) {
                console.error("Errore nella chiamata AJAX: ", error);
            },
        });
    });

    /*====================================*/
    /* Gestione click nella riga          */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#rig").on("click", "td", function () {
        /*--------------------------------*/
        /* Indicatori colonne             */
        /*--------------------------------*/
        var cln_pro = 12;
        var cln_qta = cln_pro + 4;
        var cln_ubi = cln_pro + 7;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find("#pri").val();
        var ide_row = "#row_" + pri_bfo;
        /*--------------------------------*/
        /* Numero colonna                 */
        /*--------------------------------*/
        var num_cln = $(this).parent().children().index($(this));
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var alf_pro = $(ide_row).find("#alf").val();
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked")) {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spn = "S";
        } else {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spn = "N";
        }

        /*================================*/
        /* Se colonna della quantita'     */
        /* ordinata e non ci sia gia' la  */
        /* spunta                         */
        /*--------------------------------*/
        if (num_cln == cln_qta) {
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            if (flg_spn == "N") {
                var qta_bfo = $(this).closest("tr").find("#qta").val();
                $(this).closest("tr").find("#qtv").val(qta_bfo);
            }
        }

        /*================================*/
        /* Se colonna dell'ubicazione     */
        /*--------------------------------*/
        if (num_cln == cln_ubi) {
            /*----------------------------*/
            /* Parametri                  */
            /*----------------------------*/
            var row_rif = $(this).closest("tr");
            var alf_pro = row_rif.find("#alf").val();
            var num_pro = row_rif.find("#pro").val();
            var ubi_prh = row_rif.find("#ubp").val();
            var rsp_des = $("#rsp_des").val();
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: url_exu,
                contentType: "application/x-www-form-urlencoded",
                data: { alf_pro: alf_pro },
                success: function (data) {
                    $("#dialog").html(data);
                    $("#dialog")
                        .dialog({
                            title: `Prodotto: ${alf_pro}`,
                            open: function () {
                                /*----------------*/
                                /* Assegna evento */
                                /* click primo td */
                                /*----------------*/
                                $("#dialog table tr td:first-child").on(
                                    "click",
                                    function () {
                                        var ubi_pri = $(this).text().trim();
                                        /*------------*/
                                        /* Ubicazioni */
                                        /* di trans.  */
                                        /*------------*/
                                        if (
                                            ubi_prh === ubi_pri ||
                                            ubi_non.includes(ubi_pri)
                                        ) {
                                            return;
                                        }
                                        /*------------*/
                                        /* Tipo       */
                                        /* operazione */
                                        /*------------*/
                                        var tip_ope = "UP";
                                        /*------------*/
                                        /* Conferma   */
                                        /*            */
                                        /* Funzione   */
                                        /* 'eleubiJ0' */
                                        /*------------*/
                                        dlg_ubi(
                                            tip_ope,
                                            ubi_pri,
                                            num_pro,
                                            rsp_des
                                        );
                                    }
                                );
                            },
                        })
                        .dialog("open");
                },
            });
        }
        /*------------------------------------*/
        /* Gestione click nella riga - fine   */
        /*                                    */
        /* Livello '1'                        */
        /*====================================*/
    });

    /*====================================*/
    /* Highlight prodotto                 */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $(document).on("click", ".numbersOnly", function () {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select();
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        cel_css("cel_num", "+");
    });

    /*====================================*/
    /* Rilevazione tasto funzione Tangram */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $(".numbersOnly").keyup(function (e) {
        /*--------------------------------*/
        /* Rilevazione tasto              */
        /*--------------------------------*/
        var tgk = e.keyCode ? e.keyCode : e.which;
        if (tgk == 13) {
            $(mod_frm).submit();
        }
    });

    /*====================================*/
    /* Sblocco checkbox dopo pressione    */
    /* lunga (4 secondi) su una cella     */
    /*====================================*/
    var pressTimer = null;

    $("#rig").on("mousedown", "td#cel_qtv", function (e) {
        console.log("click");
        /*--------------------------------*/
        /* Riferimento alla riga          */
        /*--------------------------------*/
        var $row = $(this).closest("tr");
        /*--------------------------------*/
        /* Riferimento alla checkbox      */
        /*--------------------------------*/
        var $chk = $row.find('input[type="checkbox"]');
        /*--------------------------------*/
        /* Solo se la checkbox è          */
        /* disabilitata                   */
        /*--------------------------------*/
        if ($chk.prop("disabled")) {
            /*----------------------------*/
            /* Avvio timer 4 secondi      */
            /*----------------------------*/
            pressTimer = setTimeout(function () {
                $("#dialog")
                    .dialog("option", "title", "Spunta Tolta")
                    .dialog("open");
                /*------------------------*/
                /* Sblocco checkbox       */
                /*------------------------*/
                $chk.prop("disabled", false);
                /*------------------------*/
                /* Togli spunta alla      */
                /* checkbox               */
                /*------------------------*/
                $chk.prop("checked", false);
                /*------------------------*/
                /* Sblocco campi correlati*/
                /*------------------------*/
                $row.find("#qtv").prop("disabled", false);
                $row.find("#ann").prop("disabled", false);
                /*----------------------------*/
                /* Forza click sulla checkbox */
                /*----------------------------*/
                // $chk.trigger('click');
            }, 4000);
        }
    });
    /*------------------------------------*/
    /* Se rilascia prima annullo tutto    */
    /*------------------------------------*/
    $("#rig").on("mouseup mouseleave", "td#cel_qtv", function (e) {
        clearTimeout(pressTimer);
    });
    /*====================================*/
    /* Modifica box di spunta             */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#rig").on("change", 'input[type="checkbox"]', function () {
        /*--------------------------------*/
        /* Riferimento alla check         */
        /*--------------------------------*/
        var $chk = $(this);
        var pre_sts = !$chk.prop("checked"); // ⬅️ questo è lo stato PRIMA della modifica
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WC";
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find("#pri").val();
        var ide_row = "#row_" + pri_bfo;
        var ncf_row = "#rw2_" + pri_bfo;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find("#prt").val();
        var prg_bfo = $(ide_row).find("#prg").val();
        var qta_bfo = $(ide_row).find("#qta").val();
        var qtv_bfo = $(ide_row).find("#qtv").val();
        var col_bfo = $(ide_row).find("#prc").val();
        /*--------------------------------*/
        /* Controllo quantità inserita    */
        /*--------------------------------*/
        if (parseFloat(qtv_bfo) > parseFloat(qta_bfo)) {
            alert(parseFloat(qtv_bfo) + " > " + parseFloat(qta_bfo));
            alert("Quantità superiore a quella ");
            $chk.prop("checked", pre_sts);
            return;
        } else if (qtv_bfo === "0") {
            alert("QUANTITA' INSERITA A 0");
        }
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked")) {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spn = "S";
            /*----------------------------*/
            /* Disabilitazione input      */
            /*----------------------------*/
            $(ide_row).find("#qtv").prop("disabled", true);
            $(ide_row).find("#ann").prop("disabled", true);
            $(ide_row).find('input[type="checkbox"]').prop("disabled", true);
        } else {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spn = "N";
            /*----------------------------*/
            /* Abilitazione input         */
            /*----------------------------*/
            $(ide_row).find("#qtv").prop("disabled", false);
            $(ide_row).find("#ann").prop("disabled", false);
        }

        /*--------------------------------*/
        /* Dialog per la conferma         */
        /*--------------------------------*/
        cnf_dlg(
            "Confermi la spunta?",
            function () {
                /*------------------------*/
                /* Funzione con_frm       */
                /*------------------------*/
                inv_spn(
                    rsp_doc,
                    tip_ope,
                    prt_bfo,
                    prg_bfo,
                    pri_bfo,
                    qta_bfo,
                    qtv_bfo,
                    col_bfo,
                    ncf_row,
                    flg_spn
                )
                    /*---------------------*/
                    /* Controllo per stampa*/
                    /*---------------------*/
                    .then(function () {
                        return ctr_stp(prt_bfo);
                    })
                    /*--------------------*/
                    /* Cambio colore      */
                    /* stampante          */
                    /*--------------------*/
                    .then(function (flag) {
                        flg_stp(flag);
                        $("#alf_pro").focus();
                        $("#alf_pro").select();
                    })
                    .catch(function (error) {
                        console.error("Errore in ctr_stp:", error);
                    });
            },
            function () {
                /*------------------------*/
                /* Funzione can_cel       */
                /*------------------------*/
                $chk.prop("checked", false);
                $(ide_row).find("#qtv").prop("disabled", false);
                $(ide_row)
                    .find('input[type="checkbox"]')
                    .prop("disabled", false);
                $("#alf_pro").focus();
                $("#alf_pro").select();
            }
        );
    });

    /*====================================*/
    /* Modifica quantita'                 */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $(document).on("change", ".numbersOnly", function () {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WC";
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find("#pri").val();
        var ide_row = "#row_" + pri_bfo;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find("#prt").val();
        var prg_bfo = $(ide_row).find("#prg").val();
        var qta_bfo = $(ide_row).find("#qta").val();
        var qtv_bfo = $(ide_row).find("#qtv").val();
        var col_bfo = $(ide_row).find("#prc").val();
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked")) {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spn = "S";
            /*----------------------------*/
            /* Disabilitazione input      */
            /*----------------------------*/
            $(ide_row).find("#qta").prop("disabled", true);
            $(ide_row).find("#ann").prop("disabled", true);
        } else {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spn = "N";
            /*----------------------------*/
            /* Abilitazione input         */
            /*----------------------------*/
            $(ide_row).find("#qta").prop("disabled", false);
            $(ide_row).find("#ann").prop("disabled", false);
        }

        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm = {
            rsp_doc: rsp_doc,
            tip_ope: tip_ope,
            prt_bfo: prt_bfo,
            prg_bfo: prg_bfo,
            pri_bfo: pri_bfo,
            qta_bfo: qta_bfo,
            qtv_bfo: qtv_bfo,
            col_bfo: col_bfo,
            flg_spn: flg_spn,
        };
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm);

        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: url_rc0,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data) {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                //$('#msg').html(data)      ;
            },
        });
    });

    /*====================================*/
    /* Bottone Non conformita'            */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $(document).on("click", ".ncf", function () {
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find("#pri").val();
        var ide_rw2 = "#rw2_" + pri_bfo;
        /*--------------------------------*/
        /* Apertura / chiusura riga       */
        /*--------------------------------*/
        $(ide_rw2).toggle();
        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({ "background-color": "#FFFFFF" });
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        $(this)
            .closest("tr")
            .find("#cel_ncf")
            .css({ "background-color": "#BBBBBB" });
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var ide_rw4 = "#rw4_" + pri_bfo;
        /*--------------------------------*/
        /* Apertura / chiusura riga       */
        /*--------------------------------*/
        $(ide_rw4).toggle();
        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({ "background-color": "#FFFFFF" });
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        $(this)
            .closest("tr")
            .find("#cel_ncf")
            .css({ "background-color": "#BBBBBB" });
    });

    /*====================================*/
    /* Accettazione testo Non conformita' */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $(document).on("change", ".inp_ncf", function () {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WN";
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find("#pri").val();
        var ide_row = "#rw2_" + pri_bfo;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find("#prt").val();
        var prg_bfo = $(ide_row).find("#prg").val();
        var pri_bfo = $(ide_row).find("#pri").val();
        var ann_ncf = $(ide_row).find("#nct").val();
        var col_bfo = $(ide_row).find("#prc").val();
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm = {
            rsp_des: rsp_des,
            tip_ope: tip_ope,
            prt_bfo: prt_bfo,
            prg_bfo: prg_bfo,
            pri_bfo: pri_bfo,
            col_bfo: col_bfo,
            ann_ncf: ann_ncf,
        };
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm);
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: url_rc0,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data) {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                $("#msg").html(data);
            },
        });
    });

    /*====================================*/
    /* Accettazione Barcode               */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $(document).on("change", ".acc_klb", function () {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WB";
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find("#pri").val();
        var ide_row = "#rw3_" + pri_bfo;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find("#prt").val();
        var prg_bfo = $(ide_row).find("#prg").val();
        var pri_bfo = $(ide_row).find("#pri").val();
        var klb_pro = $(ide_row).find("#klb").val();
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm = {
            rsp_des: rsp_des,
            tip_ope: tip_ope,
            prt_bfo: prt_bfo,
            prg_bfo: prg_bfo,
            pri_bfo: pri_bfo,
            klb_pro: klb_pro,
        };
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm);
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: url_rc0,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data) {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                $("#msg").html(data);
            },
        });
    });

    /*====================================*/
    /* Bottone stampa                     */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $(document).on("click", "#stp", function () {
        /*--------------------------------*/
        /* Protocollo                     */
        /*--------------------------------*/
        var prt_bfo = $("#prt").val();
        /*--------------------------------*/
        /* Messaggio di stampa in corso   */
        /*--------------------------------*/
        var msg_stp =
            "<h1>Esecuzione " + "stampa documento " + "in corso ...</h1";
        $("#msg").html(msg_stp);
        /*--------------------------------*/
        /* Attesa di pochi secondi        */
        /*--------------------------------*/
        $("#stp").slideUp(1200).fadeIn(1200);
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm = {
            rsp_doc: rsp_doc,
            prt_bfo: prt_bfo,
        };
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm);
        /*--------------------------------*/
        /* Stampa                         */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo di stampa  */
            /*----------------------------*/
            type: "POST",
            url: url_stp,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andata a buon fine      */
            /*----------------------------*/
            success: function (data) {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                $("#msg").html(data);
            },
        });
    });

    /*----------------------------------------*/
    /* Fine funzione livello '1'              */
    /*========================================*/
}

/*========================================*/
/* FUNZIONI                               */
/*========================================*/

/*========================================*/
/* Ricerca codice prodotto nei documenti  */
/* aperti                                 */
/*----------------------------------------*/
function ric_pro(alf) {
    /*------------------------------------*/
    /* Test su codice prodotto            */
    /*------------------------------------*/
    if (alf == "") {
        return;
    }
    /*------------------------------------*/
    /* Ricerca prodotto                   */
    /*------------------------------------*/
    $.ajax({
        type: "POST",
        url: url_pr1,
        contentType: "application/x-www-form-urlencoded",
        data: "rsp_doc=" + rsp_doc + "&alf_pro=" + alf,
        /*--------------------------------*/
        /* Esito lettura                  */
        /*--------------------------------*/
        success: function (data) {
            /*----------------------------*/
            /* Normalizzazione valore     */
            /*----------------------------*/
            var exi = data.trim();
            /*----------------------------*/
            /* Se non trovato nessun do-  */
            /* cumento                    */
            /*----------------------------*/
            if (exi == "ND") {
                alert("Nessun documento aperto per il prodotto!");
            } else if (exi == "NE") {
                alert("Codice prodotto non trovato!");
            } else {
                /*----------------------------*/
                /* Se trovato documenti       */
                /*----------------------------*/
                $("#dialog").html(data);
                $("#dialog")
                    .dialog("option", "title", "Documenti di carico")
                    .dialog("open");
            }
        },
    });

    /*----------------------------------------*/
    /* Ricerca codice prodotto - fine         */
    /*========================================*/
}

/*========================================*/
/* Funzione per generare righe nella      */
/* tabella                                */
/*----------------------------------------*/
function fil_tab(rows) {
    /*------------------------------------*/
    /* Selezione e pulizia del <tbody>    */
    /*------------------------------------*/
    var tbody = $("#rig tbody");
    if (tbody.length === 0) {
        console.error(
            "Elemento tbody non trovato. Controlla l'HTML della tabella."
        );
        return;
    }
    tbody.empty();
    /*------------------------------------*/
    /* Ordinamento delle righe per numero */
    /*------------------------------------*/
    var row_num = Object.keys(rows).sort(function (a, b) {
        return parseInt(a) - parseInt(b);
    });
    /*------------------------------------*/
    /* Definizione ordine delle colonne   */
    /*------------------------------------*/
    var order = [
        "alf_pro", // Codice prodotto
        "des_pro", // Descrizione prodotto
        "cdp_pdt", // Codice produttore
        "num_ctn", // Numero CTN
        "qta_ord", // Quantita' ordinata
        "qta_vrf", // Quantita' verificata
        "flg_spn", // Flag spunta
        "ubi_pro", // Ubicazione
        "qta_gia", // Quantita' in giacenza
    ];
    /*------------------------------------*/
    /* Definizione della mappatura dei    */
    /* campi per visualizzazione e campi  */
    /* hidden                             */
    /* __MANCA L'UBICAZIONE PRINCIPALE__  */
    /*------------------------------------*/
    var fld_map = {
        alf_pro: { visible: true, hidden: true },
        des_pro: { visible: true, hidden: false },
        cdp_pdt: { visible: true, hidden: false },
        num_ctn: { visible: true, hidden: false },
        qta_ord: { visible: true, hidden: true },
        qta_vrf: { visible: true, hidden: false },
        flg_spn: { visible: true, hidden: false },
        ubi_vrt: { visible: false, hidden: true },
        qta_gia: { visible: true, hidden: false },
        des_rsm: { visible: false, hidden: true },
        num_doc: { visible: false, hidden: true },
        num_prt: { visible: false, hidden: true },
        dat_doc: { visible: false, hidden: true },
        cod_fnt: { visible: false, hidden: true },
        num_dst: { visible: false, hidden: true },
        lit_dst: { visible: false, hidden: true },
        prt_mag: { visible: false, hidden: true },
        num_prg: { visible: false, hidden: true },
        num_prc: { visible: false, hidden: true },
        num_pro: { visible: false, hidden: true },
        bcd_pro: { visible: false, hidden: true },
        ubi_pro: { visible: true, hidden: true },
        ubi_pri: { visible: false, hidden: true },
    };
    /*------------------------------------*/
    /* Definizione della mappatura per i  */
    /* campi hidden: per ogni campo del   */
    /* JSON, specifica il nuovo name e    */
    /* id da utilizzare                   */
    /*------------------------------------*/
    var hid_map = {
        des_rsm: { name: "rsm", id: "rsm" },
        num_prt: { name: "prt", id: "prt" },
        num_prg: { name: "prg", id: "prg" },
        qta_ord: { name: "qta", id: "qta" },
        bcd_pro: { name: "bcd", id: "bcd" },
        alf_pro: { name: "alf", id: "alf" },
        num_pro: { name: "pro", id: "pro" },
        ubi_pri: { name: "ubp", id: "ubp" },
        num_prc: { name: "prc", id: "prc" },
    };

    /*------------------------------------*/
    /* Definizione dei template per i     */
    /* campi personalizzati               */
    /*------------------------------------*/
    var fld_tpl = {
        alf_pro: `<td align="left">{value}</td>`,
        des_pro: `<td align="left">{value}</td>`,
        cdp_pdt: `<td align="right">{value}</td>`,
        num_ctn: `<td align="right">{value}</td>`,
        qta_ord: `<td align="right">{value}</td>`,
        qta_vrf: `<td id="cel_qtv" name="cel_qtv">
                        <input type="number" id="qtv" name="qtv" class="numbersOnly" 
                            style="text-align:center;font-weight:bold; color:red;" 
                            value="{value}" min="0" max="999999">
                        </td>`,
        flg_spn: `<td id="cel_spn" name="cel_spn" align="center" class="td_nb">
                        <input type="checkbox" id="{rowNum}_000">
                        </td>`,
        qta_gia: `<td align="right">{value}</td>`,
        ubi_pro: `<td id="lit_ubi" name="lit_ubi" style="font-weight: bold;">{value}</td>`,
    };
    /*------------------------------------*/
    /* Calcola il numero totale di colonne*/
    /* della riga principale              */
    /*------------------------------------*/
    var tot_col = order.length + 2;
    /*------------------------------------*/
    /* Generazione delle righe della      */
    /* tabella                            */
    /*------------------------------------*/
    row_num.forEach(function (rowNum) {
        /*--------------------------------*/
        /* Recupera l'oggetto dati per la */
        /* riga corrente                  */
        /*--------------------------------*/
        var rowObj = rows[rowNum];
        /*--------------------------------*/
        /* Genera i campi hidden per la   */
        /* riga corrente                  */
        /*--------------------------------*/
        var fld_hid = "";
        Object.keys(fld_map).forEach(function (field) {
            if (
                fld_map[field].hidden === undefined ||
                fld_map[field].hidden === true
            ) {
                var value = rowObj[field] !== undefined ? rowObj[field] : "";
                /*------------------------*/
                /* Se esiste un mapping   */
                /* usa i nuovi name e id  */
                /*------------------------*/
                if (hid_map.hasOwnProperty(field)) {
                    fld_hid +=
                        '<input type="hidden" name="' +
                        hid_map[field].name +
                        '" id="' +
                        hid_map[field].id +
                        '" value="' +
                        value +
                        '">';
                }
            }
        });
        /*--------------------------------*/
        /* Inizializza la riga con un id  */
        /* univoco                        */
        /*--------------------------------*/
        var row = '<tr id="row_' + pad(rowNum, 3) + '">';
        /*--------------------------------*/
        /* Controllo se il valore di      */
        /* bcd_pro è presente, in caso    */
        /* assegno "S" altrimenti assegno */
        /* "N"                            */
        /*--------------------------------*/
        var snx_bcd =
            rowObj["bcd_pro"] !== undefined && rowObj["bcd_pro"].trim() !== ""
                ? "S"
                : "N";
        /*--------------------------------*/
        /* Genera il campo hidden con     */
        /* name e id personalizzati       */
        /*--------------------------------*/
        fld_hid +=
            '<input type="hidden" name="snb"' +
            'id="snb" value="' +
            snx_bcd +
            '">';
        /*--------------------------------*/
        /* Genera il campo hidden con     */
        /* name e id personalizzati       */
        /*--------------------------------*/
        fld_hid +=
            '<input type="hidden" name="pri"' +
            'id="pri" value="' +
            pad(rowNum, 3) +
            '">';
        /*--------------------------------*/
        /* Aggiungo i campi input alla    */
        /* riga                           */
        /*--------------------------------*/
        row += fld_hid;
        /*--------------------------------*/
        /* Prima colonna:                 */
        /* Numero progressivo             */
        /*--------------------------------*/
        row += '<td align="center">' + rowNum + "</td>";
        /*--------------------------------*/
        /* Creazione delle celle per ogni */
        /* campo secondo l'ordine definito*/
        /* nell'array "order"             */
        /*--------------------------------*/
        order.forEach(function (field) {
            /*----------------------------*/
            /* Recupera il valore del     */
            /* campo, se esistente        */
            /*----------------------------*/
            var value = rowObj[field] !== undefined ? rowObj[field] : "";
            if (fld_map[field].visible) {
                if (fld_tpl[field]) {
                    /*------------------------*/
                    /* Se esiste un template  */
                    /* personalizzato,        */
                    /* sostituisce            */
                    /* i placeholder          */
                    /*------------------------*/
                    var tpl = fld_tpl[field]
                        .replace(/{value}/g, value)
                        .replace(/{rowNum}/g, pad(rowNum, 3));
                    /*------------------------*/
                    /* Se il campo di flag è  */
                    /* "S" aggiunge           */
                    /* l'attributo checked    */
                    /*------------------------*/
                    if (field === "flg_spn" && value.trim() === "S") {
                        tpl = tpl.replace(
                            '<input type="checkbox"',
                            '<input type="checkbox" checked disabled'
                        );
                    }
                    row += tpl;
                } else {
                    /*------------------------*/
                    /* Altrimenti, crea una   */
                    /* cella standard con il  */
                    /* valore                 */
                    /*------------------------*/
                    row += '<td align="center">' + value + "</td>";
                }
            }
        });
        /*--------------------------------*/
        /* Aggiunge la cella fissa per    */
        /* il bottone "Non Conformita'"    */
        /*--------------------------------*/
        row += `<td id="cel_ncf" name="cel_ncf" class="td_nb">
                        <button type="button" class="ncf">
                            <img src="../icons/new.png" width="40">
                        </button>
                </td>`;
        /*--------------------------------*/
        /* Chiude la riga e la aggiunge   */
        /* al tbody                       */
        /*--------------------------------*/
        row += `</tr>`;
        tbody.append(row);
        /*--------------------------------*/
        /* Determina se la riga NC deve   */
        /* essere nascosta o meno         */
        /*--------------------------------*/
        var display_ncf =
            rowObj && rowObj.ann_ncf && rowObj.ann_ncf !== ""
                ? "display:table-row;"
                : "display:none;";
        /*--------------------------------*/
        /* Normalizzo la variabile con    */
        /* testo non conformita' se vuota  */
        /* o undiefined                   */
        /*--------------------------------*/
        var ann_ncf = rowObj.ann_ncf || "";
        ann_ncf = decodeURIComponent(ann_ncf.replace(/\+/g, " "));
        /*--------------------------------*/
        /* Generazione riga NC            */
        /*--------------------------------*/
        var ext_rw1 = `<tr class="exp_ncf" id="rw2_${pad(
            rowNum,
            3
        )}" style=${display_ncf}>
                            ${fld_hid}
                            <td colspan="${tot_col}" style="background-color: rgb(255, 255, 255);">
                                <input type="text" id="nct" 
                                       name="nct" class="inp_ncf" 
                                       value="${ann_ncf}" 
                                       maxlength="80">
                            </td>
                         </tr>`;
        tbody.append(ext_rw1);
        /*--------------------------------*/
        /* Generazione riga Barcode       */
        /*--------------------------------*/
        var ext_rw2 = `<tr class="exp_bcd" id="rw3_${pad(rowNum, 3)}">
                            ${fld_hid}
                            <td colspan="${tot_col}">
                                <b>Barcode : </b>
                                <input type="text" id="klb" 
                                       name="klb" class="acc_klb" 
                                       value="${rowObj["bcd_pro"]}" 
                                       maxlength="20">
                            </td>
                        </tr>`;
        tbody.append(ext_rw2);
    });
    /*----------------------------------------*/
    /* Generazione righe - fine               */
    /*========================================*/
}
/*========================================*/
/* Funzione per separazione delle righe   */
/* e aggiunta variabili globali           */
/*----------------------------------------*/
function sep_rig(obj) {
    /*------------------------------------*/
    /* Verifica se l'oggetto è un array   */
    /*------------------------------------*/
    if (Array.isArray(obj)) {
        /*--------------------------------*/
        /* Prende il primo oggetto        */
        /* dell'array                     */
        /*--------------------------------*/
        if (obj.length > 0) {
            obj = obj[0];
        } else {
            console.error("L'array è vuoto.");
            return;
        }
    }
    /*====================================*/
    /* Raggruppamento dei dati per riga   */
    /*------------------------------------*/
    /*------------------------------------*/
    /* Oggetto per raggruppare i dati     */
    /* di ogni riga                       */
    /*------------------------------------*/
    var rows = {};
    var glb_fld = {};
    /*------------------------------------*/
    /*Itera su ogni proprieta' dell'oggetto*/
    /*------------------------------------*/
    for (var key in obj) {
        /*--------------------------------*/
        /* Prende ogni valore chiave con  */
        /* valore, lo pulisce e crea      */
        /* un oggetto per ogni riga con   */
        /* le coppie chiave valore        */
        /*--------------------------------*/
        if (!obj.hasOwnProperty(key)) continue;
        var trm_key = key.trim();
        /*--------------------------------*/
        /* Controlla che segua il pattern */
        /* n.campo                        */
        /*--------------------------------*/
        var match = trm_key.match(/^(\d+)\.(.+)$/);
        if (match) {
            var rowNum = match[1];
            var fieldName = match[2].trim();
            if (!rows[rowNum]) {
                rows[rowNum] = {};
            }
            /*----------------------------*/
            /* Crea il nuovo oggetto      */
            /*----------------------------*/
            rows[rowNum][fieldName] = obj[key];
        } else {
            /*----------------------------*/
            /* Estrazione campi globali   */
            /*----------------------------*/
            glb_fld[trm_key] = obj[key];
        }
    }

    /*------------------------------------*/
    /* Aggiunge i campi globali ad ogni   */
    /* riga                               */
    /*------------------------------------*/
    for (var r in rows) {
        if (rows.hasOwnProperty(r)) {
            for (var g in glb_fld) {
                if (glb_fld.hasOwnProperty(g)) {
                    /*----------------------------*/
                    /* Se campo non presente      */
                    /*----------------------------*/
                    if (rows[r][g] === undefined) {
                        rows[r][g] = glb_fld[g];
                    }
                }
            }
        }
    }

    /*------------------------------------*/
    /* Ritorno le righe composte          */
    /*------------------------------------*/
    return rows;

    /*----------------------------------------*/
    /* Separazione righe - fine               */
    /*========================================*/
}

/*========================================*/
/* Generazione righe per info documento   */
/*----------------------------------------*/
function fil_inf(rows) {
    var $tbody = $("#rif tbody");
    /*------------------------------------*/
    /* Estrazione valori per riempimento  */
    /* tabella                            */
    /*------------------------------------*/
    var fld = rows[1];
    /*------------------------------------*/
    /* Costruisco la stringa con i dati   */
    /* per ogni riga e cella              */
    /*------------------------------------*/
    var htm_inf = `
        <tr>
            <td align="right">BFO </td>
            <td align="left">${fld.num_doc || ""} del ${fld.drg_mag || ""}</td>
        </tr>
        <tr>
            <td align="right">Protocollo </td>
            <td align="left">${fld.prt_doc || ""}</td>
        </tr>
        <tr>
            <td align="right">Distinta ${fld.num_dst || ""}</td>
            <td align="left">${fld.lit_dst || ""}</td>
        </tr>
        <tr>
            <td align="right">${fld.des_rsm || ""}</td>
            <td align="left">${fld.prt_mag || ""} - ${fld.drg_mag || ""}</td>
        </tr>`;

    /*------------------------------------*/
    /* Svuoto e aggiungo la riga al tbody */
    /* della tabella informativa          */
    /*------------------------------------*/
    $tbody.empty();
    $tbody.append(htm_inf);
    /*----------------------------------------*/
    /* Generazione righe informative - fine   */
    /*========================================*/
}

/*========================================*/
/* Funzione                               */
/* Colorazione di rosso ubicazione        */
/* principale ed eventuale evidenza       */
/* di codice e descrizione in presenza    */
/* dei codici ubicazione 'B'              */
/*                                        */
/* (Giorgio 12/06/24)                     */
/*                                        */
/* Scansione di tutte le righe            */
/*                                        */
/*----------------------------------------*/
function ubi_ros() {
    /*====================================*/
    /* Recupero valori della variabile    */
    /* hidden in array                    */
    /*------------------------------------*/
    var ubi_vrt = $("#ubi_vrt").val();
    var ubi_non = ubi_vrt.split(",").map((ubi) => ubi.trim());
    /*====================================*/
    /* Ciclo per ogni riga della tabella  */
    /*------------------------------------*/
    $("#rig tr").each(function () {
        /*================================*/
        /* Riferimento alla riga          */
        /*--------------------------------*/
        var $row = $(this);
        /*================================*/
        /* Estrazione literal ubicazione  */
        /*--------------------------------*/
        var lit_ubi = $row.find("#lit_ubi").text();
        /*================================*/
        /* Creazione array dal literal    */
        /*--------------------------------*/
        var ubi_arr = lit_ubi.split(",");
        /*--------------------------------*/
        /* Controllo ubicazioni che       */
        /* iniziano con 'B'               */
        /*--------------------------------*/
        var ubi_bel = ubi_arr.some(function (e) {
            return e.trim().toLowerCase().startsWith("b");
        });
        /*--------------------------------*/
        /* Se trovato iniziale 'B'        */
        /*--------------------------------*/
        if (ubi_bel) {
            /*----------------------------*/
            /* Cambia il background delle */
            /* celle codice e descrizione */
            /*----------------------------*/
            $row.find("td").eq(1).css("background-color", "lightblue");
            $row.find("td").eq(2).css("background-color", "lightblue");
        }

        /*================================*/
        /* Estrazione ubicazione princi-  */
        /* pale                           */
        /*--------------------------------*/
        var pri_ubi = $row.find("#ubp").val();
        /*--------------------------------*/
        /* Pulizia literal ubicazione     */
        /*--------------------------------*/
        lit_ubi = lit_ubi.trim();
        /*--------------------------------*/
        /* Pulizia literal ubicazione     */
        /*--------------------------------*/
        if (lit_ubi !== "") {
            /*----------------------------*/
            /* Individuazione eventuale   */
            /* ubicazione principale      */
            /*----------------------------*/
            if (lit_ubi.includes(pri_ubi)) {
                /*------------------------*/
                /* Sostituzione ubicazione*/
                /* principale             */
                /*------------------------*/
                var inx_wrd = lit_ubi.replace(
                    pri_ubi,
                    `<span style="color: red;">${pri_ubi}</span>`
                );
                lit_ubi = inx_wrd;
                /*------------------------*/
                /* Scrittura nuova        */
                /* stringa con ubicazioni */
                /* in grassetto           */
                /*------------------------*/
                $row.find("#lit_ubi").html(lit_ubi);
                $row.find("#lit_ubi").css("font-weight", "bold");
            }
        } else {
            /*----------------------------*/
            /* Stringa vuota              */
            /*----------------------------*/
            $row.find("#lit_ubi").html("");
        }
        /*================================*/
        /* Ciclo per lettura righe ed     */
        /* eventuale colorazione grigia   */
        /*--------------------------------*/
        ubi_arr.forEach(function (e) {
            /*----------------------------*/
            /* Rimuovo caratteri laterali */
            /*----------------------------*/
            var ubi_trm = e.trim();
            /*----------------------------*/
            /* Se valore corrisponde a non*/
            /* modificabili, cambia colore*/
            /*----------------------------*/
            if (ubi_non.includes(ubi_trm.toUpperCase())) {
                /*------------------------*/
                /* Sostituisco valore     */
                /* colorato in grigio     */
                /*------------------------*/
                var inx_wrd = lit_ubi.replace(
                    ubi_trm,
                    `<span style="color: grey;">${ubi_trm}</span>`
                );
                lit_ubi = inx_wrd;
                /*------------------------*/
                /* Scrittura nuova        */
                /* stringa con ubicazioni */
                /* in grassetto           */
                /*------------------------*/
                $row.find("#lit_ubi").html(lit_ubi);
                $row.find("#lit_ubi").css("font-weight", "bold");
            }
        });

        /*------------------------------------*/
        /* Colorazione di rosso ubicazione    */
        /*                                    */
        /* Fine                               */
        /*====================================*/
    });
}

/*========================================*/
/* Funzione generica di switch colore     */
/* cella                                  */
/*----------------------------------------*/
function cel_css(field, swc) {
    /*------------------------------------*/
    /* Normalizzazione variabile colore   */
    /*------------------------------------*/
    var color = "";
    /*------------------------------------*/
    /* Colore in funzione dello switch    */
    /*------------------------------------*/
    if (swc == "-") {
        color = "BBBBBB";
    } else if (swc == "+") {
        color = "777777";
    } else if (swc == "R") {
        color = "FF0000";
    } else if (swc == "G") {
        color = "00FF00";
    } else if (swc == "W") {
        color = "FFFFFF";
    } else {
        color = "BBBBBB";
    }
    /*------------------------------------*/
    /* Aggiornamento sfondo cella         */
    /*------------------------------------*/
    $("#" + field).css("background-color", "#" + color);
    /*----------------------------------------*/
    /* Fine funzione                          */
    /*========================================*/
}

/*========================================*/
/* Funzione di pulizia stringa            */
/*                                        */
/* N.B.: usata per togliere caratteri non */
/*       alfabetici o numerici            */
/*----------------------------------------*/
function pul_str(inp_str) {
    return inp_str.replace(/[^a-zA-Z0-9]/g, "");
}

/*========================================*/
/* Funzione per rendere numero riga con   */
/* 0 iniziali                             */
/*----------------------------------------*/
function pad(num, size) {
    var s = String(num);
    while (s.length < size) s = "0" + s;
    return s;
}
/*========================================*/
/* Funzione per pulire le chiavi di un    */
/* oggetto                                */
/*----------------------------------------*/
function trimKeys(obj) {
    var newObj = {};
    Object.keys(obj).forEach(function (key) {
        newObj[key.trim()] = obj[key];
    });
    return newObj;
}

/*========================================*/
/* Funzione per disabilitare campi input  */
/* delle righe gia spuntate               */
/*----------------------------------------*/
function dis_inp() {
    /*------------------------------------*/
    /* Passo in rassegna ogni riga        */
    /*------------------------------------*/
    $("#rig")
        .find('input[type="checkbox"]')
        .each(function () {
            /*--------------------------------*/
            /* Se il checkbox è spuntato,     */
            /* disabilita l'input quantita'    */
            /* corrispondente                 */
            /*--------------------------------*/
            if ($(this).is(":checked")) {
                $(this).closest("tr").find("#qtv").prop("disabled", true);
            }
        });
}

/*========================================*/
/* Funzione per ordinare ed evidenziare   */
/* la tabella in base al codice prodotto  */
/* includendo le righe accessorie         */
/*----------------------------------------*/
function hig_rig(rec) {
    /*------------------------------------*/
    /* Se il codice è vuoto, rimuovo la   */
    /* classe "sel" dalle righe principali*/
    /*------------------------------------*/
    if (rec === "") {
        $('#rig tbody tr[id^="row_"]').removeClass("sel");
        return;
    }
    /*------------------------------------*/
    /* Seleziono solo le righe principali */
    /* (quelle con id che iniziano con    */
    /* "row_")                            */
    /*------------------------------------*/
    const $mainRows = $('#rig tbody tr[id^="row_"]');

    /*------------------------------------*/
    /* Raggruppo le righe principali con  */
    /* le righe accessorie successive     */
    /*------------------------------------*/
    let groups = [];
    $mainRows.each(function () {
        const $mainRow = $(this);
        /*--------------------------------*/
        /* Recupera il codice prodotto    */
        /* dalla cella identificata da    */
        /* "#alf"                         */
        /*--------------------------------*/
        const cod_row = (
            $mainRow.find("#alf").val() || $mainRow.find("#alf").text()
        ).trim();
        /*--------------------------------*/
        /* Seleziono le righe accessorie  */
        /* che seguono fino alla prossima */
        /* riga principale                */
        /*--------------------------------*/
        const $extras = $mainRow.nextUntil('tr[id^="row_"]');
        groups.push({
            code: cod_row,
            main: $mainRow,
            extras: $extras,
        });
    });

    /*------------------------------------*/
    /* Separa il gruppo corrispondente al */
    /* codice ricercato e gli altri gruppi*/
    /*------------------------------------*/
    let matchingGroup = [];
    let otherGroups = [];
    groups.forEach(function (group) {
        if (group.code === rec.trim()) {
            matchingGroup.push(group);
        } else {
            otherGroups.push(group);
        }
    });

    /*------------------------------------*/
    /* Ordino gli altri gruppi in base    */
    /* all'ID della riga principale       */
    /*------------------------------------*/
    otherGroups.sort(function (a, b) {
        const numA = parseInt(a.main.attr("id").match(/\d+/)[0]);
        const numB = parseInt(b.main.attr("id").match(/\d+/)[0]);
        return numA - numB;
    });

    /*------------------------------------*/
    /* Combino il gruppo corrispondente   */
    /* con gli altri gruppi               */
    /*------------------------------------*/
    const orderedGroups = matchingGroup.concat(otherGroups);

    /*------------------------------------*/
    /* Ricomponi il <tbody> con i gruppi  */
    /*------------------------------------*/
    const $tbody = $("#rig tbody");
    $tbody.empty();
    orderedGroups.forEach(function (group) {
        /*--------------------------------*/
        /* Evidenzio la riga principale   */
        /* se il codice corrisponde a     */
        /* "rec"                          */
        /*--------------------------------*/
        if (group.code === rec.trim()) {
            group.main.addClass("sel");
        } else {
            group.main.removeClass("sel");
        }
        /*--------------------------------*/
        /* Aggiunge la riga principale    */
        /* e le sue righe accessorie al   */
        /* <tbody>                        */
        /*--------------------------------*/
        $tbody.append(group.main);
        group.extras.each(function () {
            $tbody.append($(this));
        });
    });

    /*------------------------------------*/
    /* Ripristina focus e selezione sul   */
    /* campo di ricerca prodotto          */
    /*------------------------------------*/
    $("#alf_pro").focus();
    $("#alf_pro").select();
}
/*========================================*/
/* Funzione creazione messaggio di dialogo*/
/* utilizza callback di conferma          */
/* o cancellazione                        */
/*----------------------------------------*/
function cnf_dlg(message, con_frm, can_cel) {
    /*------------------------------------*/
    /* Creazione dell'elemento div per    */
    /*  il dialogo                        */
    /*------------------------------------*/
    var $dialog = $("<div>" + message + "</div>");

    /*------------------------------------*/
    /* Inizializzazione del dialog        */
    /*------------------------------------*/
    $dialog.dialog({
        modal: true,
        title: "Attenzione",
        resizable: false,
        /*--------------------------------*/
        /* Definizione dei pulsanti       */
        /*--------------------------------*/
        buttons: {
            Si: function () {
                /*------------------------*/
                /* Azione per confermare: */
                /* chiude dialog e invoca */
                /* funzione conferma      */
                /*------------------------*/
                $(this).dialog("close");
                if (typeof con_frm === "function") {
                    con_frm();
                }
            },
            No: function () {
                /*------------------------*/
                /* Azione per cancellare: */
                /* chiude dialog e invoca */
                /* funzione cancellazione */
                /*------------------------*/
                $(this).dialog("close");
                if (typeof can_cel === "function") {
                    can_cel();
                }
            },
        },
        /*--------------------------------*/
        /* Rimozione del div              */
        /*--------------------------------*/
        close: function () {
            $(this).remove();
        },
    });
}
/*========================================*/
/* Funzione invio dati per spunta si riga */
/*----------------------------------------*/
function inv_spn(
    rsp_doc,
    tip_ope,
    prt_bfo,
    prg_bfo,
    pri_bfo,
    qta_bfo,
    qtv_bfo,
    col_bfo,
    ncf_row,
    flg_spn
) {
    return new Promise(function (resolve, reject) {
        /*------------------------------------*/
        /* Contenuto della non conformità     */
        /*------------------------------------*/
        var $row = $(ncf_row);
        var ann_ncf = $row.find("#nct").val();
        /*------------------------------------*/
        /* Oggetto con i Parametri            */
        /*------------------------------------*/
        const prm = {
            rsp_doc: rsp_doc,
            tip_ope: tip_ope,
            prt_bfo: prt_bfo,
            prg_bfo: prg_bfo,
            pri_bfo: pri_bfo,
            qta_bfo: qta_bfo,
            qtv_bfo: qtv_bfo,
            col_bfo: col_bfo,
            ann_ncf: ann_ncf,
            flg_spn: flg_spn,
        };
        /*------------------------------------*/
        /* Assemblaggio parametri             */
        /*------------------------------------*/
        const dat = $.param(prm);
        /*====================================*/
        /* Ajax per invio spunta              */
        /*------------------------------------*/
        $.ajax({
            type: "POST",
            url: url_rc0,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*--------------------------------*/
            /* Se andato a buon fine          */
            /*--------------------------------*/
            success: function (data) {
                /*------------------------*/
                /* Controllo quantita', se*/
                /* inferiore aggiungo     */
                /* testo non conformita'  */
                /*------------------------*/
                var qta = qta_bfo - qtv_bfo;
                console.log($row.find("#nct").val());
                /*------------------------*/
                /* Se quantita' maggiore  */
                /* di 0 vuol dire che     */
                /* ci sono pezzi in meno  */
                /*------------------------*/
                if (qta > 0) {
                    if ($row.length > 0 && $row.find("#nct").val() === "") {
                        $row.find("#nct")
                            .val("Quantita' inferiore -" + qta)
                            .trigger("change");
                        /*----------------*/
                        /* Apertura riga  */
                        /*----------------*/
                        $row.toggle();
                    }
                }
                resolve(data);
            },
            /*--------------------------------*/
            /* In caso di errore              */
            /*--------------------------------*/
            error: function (err) {
                console.error("Errore in inv_spn:", err);
                reject(err);
            },
        });
    });
}
/*========================================*/
/* Funzione invio dati per spunta si riga */
/*----------------------------------------*/
function ctr_stp(prt_bfo) {
    return new Promise((resolve, reject) => {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "SS";
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm = {
            tip_ope: tip_ope,
            prt_bfo: prt_bfo,
        };
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm);
        /*================================*/
        /* Ajax per controllo stato file  */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url: url_rc1,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            success: function (data) {
                resolve(data);
            },
            error: function (error) {
                reject(error);
            },
        });
    });
}
/*========================================*/
/* Funzione invio dati per spunta si riga */
/*----------------------------------------*/
function flg_stp(flag) {
    flag = flag.trim();
    /*====================================*/
    /* Inibizione totale delle spunte se  */
    /* stampante verde, cioe' se tutte le */
    /* righe sono spuntate                */
    /*------------------------------------*/
    if (flag !== "N") {
        $("input[type=checkbox]").attr("disabled", "true");
        $("#alf_pro").attr("disabled", "disabled");
    } else {
        $("#alf_pro").prop("enabled");
    }

    /*====================================*/
    /* Eventuale switch bottone stampa    */
    /*------------------------------------*/
    if (flag === "N" || flag === undefined) {
        cel_css("stp", "R");
        $("#stp").prop("disabled", true);
    } else {
        cel_css("stp", "G");
        $("#stp").prop("disabled", false);
    }
}
