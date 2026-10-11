/*========================================*/
/* eletest1 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 10/07/2023           */
/*========================================*/

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Testata                            */
    /*------------------------------------*/

    /*------------------------------------*/
    /* Numero documento                   */
    /*------------------------------------*/
    num_doc = $('#num_doc').val()         ;
    /*------------------------------------*/
    /* Responsabile                       */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    rsp_des = $('#rsp_doc option:selected').text();
    /*------------------------------------*/
    /* Flag di stampa                     */
    /*------------------------------------*/
    flg_stp = $('#fds').val()             ;

    /*====================================*/
    /* Definizione box di dialogo         */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal: true,
        title: 'Scheda prodotto',
        width: 950,
        height: 450,
    });

    /*====================================*/
    /* Eventuale focus su contrassegno    */
    /*------------------------------------*/
    if (!num_doc)
    {
        $('#num_col').focus()             ;
        cel_css("cel_col", "+")           ;
        cel_css("cel_pro", "-")           ;
        cel_css("cel_num", "-")           ;
    }

    /*====================================*/
    /* Eventuale switch iniziale campi    */
    /*------------------------------------*/
    if (!rsp_doc || rsp_doc == 0)
    {
        /*--------------------------------*/
        /* Se responsabile da impostare:  */
        /* disabilitazioni                */
        /*--------------------------------*/
        $('#num_doc').prop("disabled",true);
        $('#num_col').prop("disabled",true);
        $('#alf_pro').prop("disabled",true);
        /*--------------------------------*/
        /* Focus su responsabile          */
        /*--------------------------------*/
        $('#rsp_doc').focus()             ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_ute", "R")           ;
    }
     else
    {
        /*--------------------------------*/
        /* Se responsabile impostato:     */
        /* abilitazioni                   */
        /*--------------------------------*/
        $('#num_doc').prop("enabled",true);
        $('#num_col').prop("enabled",true);
        $('#alf_pro').prop("enabled",true);
        cel_css("cel_col", "+")           ;
        $('#alf_pro').focus()             ;
        /*--------------------------------*/
        /* Reload automatico pagina ogni  */
        /* 30 secondi                     */
        /*--------------------------------*/
//        window.setTimeout( function()
//        {
//            $("#bfo_011").submit()        ;
//        }, 30000);
        /*--------------------------------*/
        /* Aggiornamento titolo pagina    */
        /* con responsabile e ora:min:sec */
        /*--------------------------------*/
        var dat = new Date()              ;
        var tim = dat.getHours() + ":" +
                  dat.getMinutes() + ":" +
                  dat.getSeconds()        ;
        var tit = rsp_des +
                     " (" +
                      tim +
                       ")"                ;
        $(this).attr("title", tit)        ;
    }

    /*====================================*/
    /* Eventuale switch bottone stampa    */
    /*------------------------------------*/
    if (flg_stp == "#" || flg_stp == undefined)
    {
        cel_css("stp", "R")               ;
        $('#stp').prop("disabled",true)   ;
    }
     else
    {
        cel_css("stp", "G")               ;
        $('#stp').prop("enabled",true)    ;
    }
    
    /*====================================*/
    /* Eventuale inibizione codice        */
    /* prodotto                           */
    /*------------------------------------*/
    if (!num_doc || num_doc == "")
    {
        $('#alf_pro').prop("disabled",true);
    }
     else
    {
        $('#alf_pro').prop("enabled",true);
    }
    
    /*====================================*/
    /* Highlight Responsabile             */
    /*------------------------------------*/
    $('#rsp_doc').click(function()
    {
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_ute", "+")           ;
    });
    
    /*====================================*/
    /* Highlight Contrassegno             */
    /*------------------------------------*/
    $('#num_col').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_col", "+")           ;
    });

    /*====================================*/
    /* Highlight Numero documento         */
    /*------------------------------------*/
    $('#num_doc').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_num", "+")           ;
    });
    
    /*====================================*/
    /* Highlight Prodotto                 */
    /*------------------------------------*/
    $('#alf_pro').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_pro", "+")           ;
    });

    /*====================================*/
    /* Forzatura campi numerici           */
    /* (classe: numbersOnly)              */
    /*------------------------------------*/
    $('.numbersOnly').keyup(function () { 
        this.value = this.value.replace(/[^0-9\.]/g,'');
    });

    /*====================================*/
    /* Cambio Operatore                   */
    /*------------------------------------*/
    $('#rsp_doc').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#num_doc').val('')             ;
        $('#num_col').val('')             ;
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_011").submit()            ;
    });          

    /*====================================*/
    /* Bottone reset                      */
    /*------------------------------------*/
    $("#res").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#num_doc').val('')             ;
        $('#num_col').val('')             ;
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Normalizzazione delle spunte   */
        /*--------------------------------*/
//        $('input:checkbox').attr('checked','checked');
//        $('input:checkbox').removeAttr('checked');
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_011").submit()            ;
    });

    /*====================================*/
    /* Cambio Numero contrassegno         */
    /*------------------------------------*/
    $('#num_col').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_011").submit()            ;
    });          

    /*====================================*/
    /* Cambio Numero documento            */
    /*------------------------------------*/
    $('#num_doc').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')             ;
        $('#num_col').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_011").submit()            ;
    });          

    /*====================================*/
    /* Cambio Codice prodotto             */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_011").submit()            ;
    });          

    /*====================================*/
    /* Bottone stampa                     */
    /*------------------------------------*/
    $("#stp").click(function()
    {
        /*--------------------------------*/
        /* Protocollo                     */
        /*--------------------------------*/
        var prt_bfo = $('#prt').val()     ;
        /*--------------------------------*/
        /* Messaggio di stampa in corso   */
        /*--------------------------------*/
        var msg_stp = "<h1>Esecuzione " +
                      "stampa documento " +
                      "in corso ...</h1"  ;
        $('#msg').html(msg_stp)           ;
        /*--------------------------------*/
        /* Stampa                         */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo di stampa  */
            /*----------------------------*/
            type: "POST",
            url: "./elebfo1s",
            contentType: "application/x-www-form-urlencoded",
            data: "prt_bfo=" +
                   prt_bfo,
            /*----------------------------*/
            /* Se andata a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                $('#msg').html(data);
            }
        });
    });

    /*====================================*/
    /* Controllo Submit                   */
    /*------------------------------------*/
    $("#bfo_011").submit(function()
    {    
        /*--------------------------------*/
        /* ___ EVENTUALI CONTROLLI ___    */
        /*--------------------------------*/
        
    });

    /*====================================*/
    /* Tabella righe                      */
    /*====================================*/

    /*====================================*/
    /* Controllo valore massimo campi di  */
    /* input numerici                     */
    /*------------------------------------*/
    $('input[type="number"]').on('input', function ()
    {
        var value = $(this).val()         ;
        if ((value !== '') && (value.indexOf('.') === -1))
        {
            $(this).val(Math.max(Math.min(value, 999999), 0));
        }
    });

    /*====================================*/
    /* Controllo se prodotto trovato      */
    /*------------------------------------*/
    var spr = $("#spr").val()             ;
    var prd = $('#alf_pro').val()         ;
    if ((!spr || spr == 0) && (prd != '') )
    {
        alert ("PRODOTTO '" + prd + "' NON TROVATO!");
    };

    /*====================================*/
    /* Highlight campi tabella di testata */
    /*------------------------------------*/
    $("#rig td").click(function()
    {
        $("#tes td").css({"background-color": "#BBBBBB"});
    });

    /*====================================*/
    /* Eventuale spunta generale          */
    /* ___ DA IMPLEMENTARE ___            */
    /*------------------------------------*/

    /*====================================*/
    /* Accettazione quantita' verificata: */
    /* scorciatoia per copiare quantita'  */
    /* ordinata                           */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var col_bfo = $(this).closest("tr").find('#col').val();
        var ide_row = "#row_" +
                      pri_bfo +
                      "_"     +
                      col_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var num_col = $(this).parent().children().index($(this));
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked"))
        {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spn = 'S'             ;
        }
         else
        {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spn = 'N'             ;
        }
        /*--------------------------------*/
        /* Test che sia la colonna della  */
        /* quantita' ordinata e non ci    */
        /* sia gia' la spunta             */
        /*--------------------------------*/
        if (num_col == 18)
        {
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                var qta_bfo = $(this).closest("tr").find('#qta').val();
                $(this).closest("tr").find('#qtv').val(qta_bfo);
            }
        }
         else if (num_col == 15)
        {




        var codice = $(ide_row).find('#alf').val();


        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({"background-color": "#FFFFFF"});
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        var cel = $(this).closest("td").attr('id');
        $(this).closest("tr").find('#' + cel).css({"background-color": "#BBBBBB"});


        /*--------------------------------*/
        /* Riempimento box                */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url: "./eletest0",
            contentType: "application/x-www-form-urlencoded",
            data: "alf_pro=" + codice,
            success: function (data)
            {
                  $("#dialog").html(data);
                  $("#dialog").dialog("option","title","Scheda prodotto : " + codice).dialog('open');
            }
        });










        }






    });

    /*====================================*/
    /* Bottone Non conformita'            */
    /*------------------------------------*/
    $('.ncf').click(function()
    {
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var col_bfo = $(this).closest("tr").find('#col').val();
        var ide_rw2 = "#rw2_" +
                      pri_bfo +
                      "_"     +
                      col_bfo             ;
        /*--------------------------------*/
        /* Apertura / chiusura riga       */
        /*--------------------------------*/
        $(ide_rw2).toggle()               ;
        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({"background-color": "#FFFFFF"});
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        $(this).closest("tr").find('#cel_ncf').css({"background-color": "#BBBBBB"});
    });

    /*====================================*/
    /* Accettazione testo Non conformita' */
    /*------------------------------------*/
    $('.inp_ncf').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WN"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var col_bfo = $(this).closest("tr").find('#col').val();
        var ide_row = "#rw2_" +
                      pri_bfo +
                      "_"     +
                      col_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var pri_bfo = $(ide_row).find('#pri').val();
        var col_bfo = $(ide_row).find('#col').val();
        var ann_ncf = $(ide_row).find('#nct').val();
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_des=" + rsp_des ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "prt_bfo=" + prt_bfo ;
        var prm_04 = "prg_bfo=" + prg_bfo ;
        var prm_05 = "pri_bfo=" + pri_bfo ;
        var prm_06 = "col_bfo=" + col_bfo ;
        var prm_07 = "ann_ncf=" + ann_ncf ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04 + "&" +
                  prm_05 + "&" +
                  prm_06 + "&" +
                  prm_07                  ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfow0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                $('#msg').html(data)      ;
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $("#bfo_011").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Accettazione Barcode               */
    /*------------------------------------*/
    $('.acc_klb').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WB"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var col_bfo = $(this).closest("tr").find('#col').val();
        var ide_row = "#rw3_" +
                      pri_bfo +
                      "_"     +
                      col_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var pri_bfo = $(ide_row).find('#pri').val();
        var col_bfo = $(ide_row).find('#col').val();
        var klb_pro = $(ide_row).find('#klb').val();
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_des=" + rsp_des ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "prt_bfo=" + prt_bfo ;
        var prm_04 = "prg_bfo=" + prg_bfo ;
        var prm_05 = "pri_bfo=" + pri_bfo ;
        var prm_06 = "col_bfo=" + col_bfo ;
        var prm_07 = "klb_pro=" + klb_pro ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04 + "&" +
                  prm_05 + "&" +
                  prm_06 + "&" +
                  prm_07                  ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfow0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                $('#msg').html(data)      ;
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $("#bfo_011").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Highlight ubicazioni               */
    /*------------------------------------*/
    $('.inp_ubi').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({"background-color": "#FFFFFF"});
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        var cel = $(this).closest("td").attr('id');
        $(this).closest("tr").find('#' + cel).css({"background-color": "#BBBBBB"});
    });

    /*====================================*/
    /* Modifica ubicazione                */
    /*------------------------------------*/
    $('.inp_ubi').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WU"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var col_bfo = $(this).closest("tr").find('#col').val();
        var ide_row = "#row_" +
                      pri_bfo +
                      "_"     +
                      col_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var num_pro = $(ide_row).find('#pro').val();
        var alf_pro = $(ide_row).find('#alf').val();
        /*--------------------------------*/
        /* Ubicazioni accettate           */
        /*--------------------------------*/
       var ub1_rig = fld_val (ide_row, "#acc_ub1");
       var ub2_rig = fld_val (ide_row, "#acc_ub2");
       var ub3_rig = fld_val (ide_row, "#acc_ub3");
       var ub4_rig = fld_val (ide_row, "#acc_ub4");
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_des=" + rsp_des ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "num_pro=" + num_pro ;
        var prm_04 = "alf_pro=" + alf_pro ;
        var prm_05 = "ub1_rig=" + ub1_rig ;
        var prm_06 = "ub2_rig=" + ub2_rig ;
        var prm_07 = "ub3_rig=" + ub3_rig ;
        var prm_08 = "ub4_rig=" + ub4_rig ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04 + "&" +
                  prm_05 + "&" +
                  prm_06 + "&" +
                  prm_07 + "&" +
                  prm_08                  ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfow0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
   //                $('#msg').html(data)      ;
                alert (data);
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#bfo_011").submit()    ;
            }
            ,error: function(XMLHttpRequest, textStatus, errorThrown)
            {
                console.log('error');
            }
        });
    });          

    /*====================================*/
    /* Highlight quantita'                */
    /*------------------------------------*/
    $('input[type="number"]').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({"background-color": "#FFF"});
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        var cel = $(this).closest("td").attr('id');
        $(this).closest("tr").find("#" + cel).css({"background-color": "#BBB"});
    });

    /*====================================*/
    /* Modifica quantita' ricevuta        */
    /*------------------------------------*/
    $('.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WC"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var col_bfo = $(this).closest("tr").find('#col').val();
        var ide_row = "#row_" +
                      pri_bfo +
                      "_"     +
                      col_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var col_bfo = $(ide_row).find('#col').val();
        var qta_bfo = $(ide_row).find('#qta').val();
        var qtv_bfo = $(ide_row).find('#qtv').val();
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked"))
        {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spn = 'S'             ;
            /*----------------------------*/
            /* Disabilitazione input      */
            /*----------------------------*/
            $(ide_row).find('#qta').prop("disabled",true);
            $(ide_row).find('#ann').prop("disabled",true);
        }
         else
        {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spn = 'N'             ;
            /*----------------------------*/
            /* Abilitazione input         */
            /*----------------------------*/
            $(ide_row).find('#qta').prop("disabled",false);
            $(ide_row).find('#ann').prop("disabled",false);
        }

        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_doc=" + rsp_doc ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "prt_bfo=" + prt_bfo ;
        var prm_04 = "prg_bfo=" + prg_bfo ;
        var prm_05 = "pri_bfo=" + pri_bfo ;
        var prm_06 = "col_bfo=" + col_bfo ;
        var prm_07 = "qta_bfo=" + qta_bfo ;
        var prm_08 = "qtv_bfo=" + qtv_bfo ;
        var prm_09 = "flg_spn=" + flg_spn ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04 + "&" +
                  prm_05 + "&" +
                  prm_06 + "&" +
                  prm_07 + "&" +
                  prm_08 + "&" +
                  prm_09                  ;

        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfow0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                $('#msg').html(data);
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#bfo_011").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Modifica box di spunta             */
    /*------------------------------------*/
    $('input[type="checkbox"]').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WC"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var col_bfo = $(this).closest("tr").find('#col').val();
        var ide_row = "#row_" +
                      pri_bfo +
                      "_"     +
                      col_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var col_bfo = $(ide_row).find('#col').val();
        var qta_bfo = $(ide_row).find('#qta').val();
        var qtv_bfo = $(ide_row).find('#qtv').val();
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked"))
        {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spn = 'S'             ;
            /*----------------------------*/
            /* Disabilitazione input      */
            /*----------------------------*/
            $(ide_row).find('#qta').prop("disabled",true);
            $(ide_row).find('#ann').prop("disabled",true);
        }
         else
        {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spn = 'N'             ;
            /*----------------------------*/
            /* Abilitazione input         */
            /*----------------------------*/
            $(ide_row).find('#qta').prop("disabled",false);
            $(ide_row).find('#ann').prop("disabled",false);
        }

        /*--------------------------------*/
        /* Messaggio di quantita' a zero  */
        /*--------------------------------*/
        if (qtv_bfo == "0" && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_doc=" + rsp_doc ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "prt_bfo=" + prt_bfo ;
        var prm_04 = "prg_bfo=" + prg_bfo ;
        var prm_05 = "pri_bfo=" + pri_bfo ;
        var prm_06 = "col_bfo=" + col_bfo ;
        var prm_07 = "qta_bfo=" + qta_bfo ;
        var prm_08 = "qtv_bfo=" + qtv_bfo ;
        var prm_09 = "flg_spn=" + flg_spn ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04 + "&" +
                  prm_05 + "&" +
                  prm_06 + "&" +
                  prm_07 + "&" +
                  prm_08 + "&" +
                  prm_09                  ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfow0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
//                $('#msg').html(data);
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#bfo_011").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Forzatura caratteri ASCII per il   */
    /* campo testo della Non conformita'  */
    /*------------------------------------*/
    $("#nct").on("keypress",function(e)
    {
        if((e.which >= 48 && e.which <= 90) ||
           (e.which >= 97 && e.which <= 122) ||
            e.which == 45 || e.which == 32 || e.which == 39 || 
           (e.which >= 48 && e.which <= 57 ))
        {
            return true;
        }
         else
        {
            return false;
        }
    });

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/
});

/*========================================*/
/* FUNZIONI                               */
/*========================================*/

/*========================================*/
/* Funzione generica di switch colore     */
/* cella                                  */
/*----------------------------------------*/
function cel_css (field, swc)
{
    /*------------------------------------*/
    /* Normalizzazione variabile colore   */
    /*------------------------------------*/
    var color = ''                        ;
    /*------------------------------------*/
    /* Colore in funzione dello switch    */
    /*------------------------------------*/
    if (swc == "-")
    {
        color = 'BBBBBB'                  ;
    }
     else if (swc == "+")
    {
        color = '777777'                  ;
    }
     else if (swc == "R")
    {
        color = 'FF0000'                  ;
    }
     else if (swc == "G")
    {
        color = '00FF00'                  ;
    }
     else if (swc == "W")
    {
        color = 'FFFFFF'                  ;
    }
     else
    {
        color = 'BBBBBB'                  ;
    }
    /*------------------------------------*/
    /* Aggiornamento sfondo cella         */
    /*------------------------------------*/
    $("#"+field).css("background-color",
                           "#" + color)   ;
/*----------------------------------------*/
/* Fine funzione                          */
/*========================================*/
}

/*========================================*/
/* Funzione generica determinazione       */
/* valore campo riga                      */
/*----------------------------------------*/
function fld_val (row, fld)
{
    /*------------------------------------*/
    /* Determinazione                     */
    /*------------------------------------*/
    val = $(row).find(fld).val()          ;
    /*------------------------------------*/
    /* Valore in uscita                   */
    /*------------------------------------*/
    return val                            ;
/*----------------------------------------*/
/* Fine funzione                          */
/*========================================*/
}

