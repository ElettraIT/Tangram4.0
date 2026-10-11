/*========================================*/
/* eleinv03 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 23/12/2023           */
/*========================================*/

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione elenco 'check'          */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal: true,
        title: 'Scheda prodotto',
        width: 950,
        height: 300,
    });

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* conferma spunta totale             */
    /*------------------------------------*/
    $('#dialog-confirm').dialog({
      autoOpen: false,
      resizable: false,
      height: "auto",
      width: 400,
      modal: true
    });    

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* conferma spunta totale             */
    /*                                    */
    /* Default 'NO'                       */
    /*------------------------------------*/
    $("#dialog-confirm").dialog({
        open: function()
        {
         $(this).siblings('.ui-dialog-buttonpane').find('button:eq(1)').focus(); 
        }
    });

    /*====================================*/
    /* Definizione box di dialogo -       */
    /* opzioni                            */
    /*------------------------------------*/
    $('#dialog-confirm').dialog('option', 'buttons',
    {
        /*--------------------------------*/
        /* Scelta 'SI'                    */
        /*--------------------------------*/
        "si" : function()
        {
            /*----------------------------*/
            /* Tipo operazione            */
            /*----------------------------*/
            var tip_ope = "WD"            ;
            /*----------------------------*/
            /* Identificatori riga        */
            /*----------------------------*/
            var nrr = $('#dialog-confirm').data('riga');
            var ide_row = "#row_" + nrr   ;
            /*----------------------------*/
            /* Valori da riga tabella     */
            /*----------------------------*/
            var cod_ubi = $(ide_row).find('#ubi').val();
            var alf_pro = $(ide_row).find('#alf').val();
            var num_pro = $(ide_row).find('#pro').val();
            /*============================*/
            /* Parametri                  */
            /*----------------------------*/
            const par = []                ;
            const val = []                ;
            /*----------------------------*/
            par[0] = "tip_ope"            ;
            val[0] = tip_ope              ;
            /*----------------------------*/
            par[1] = "cod_ubi"            ;
            val[1] = cod_ubi              ;
            /*----------------------------*/
            par[2] = "alf_pro"            ;
            val[2] = alf_pro              ;
            /*----------------------------*/
            par[3] = "num_pro"            ;
            val[3] = num_pro              ;
            /*----------------------------*/
            /* Assemblaggio               */
            /*----------------------------*/
            prm = ass_prm(par, val)       ;
            /*----------------------------*/
            /* Aggiornamento Tangram      */
            /*----------------------------*/
            $.ajax({
                /*----------------------------*/
                /* Richiamo modulo            */
                /*----------------------------*/
                type: "POST",
                url: "./eleinvw0",
                contentType: "application/x-www-form-urlencoded",
                data: prm,
                /*----------------------------*/
                /* Se andato a buon fine      */
                /*----------------------------*/
                success: function (data)
                {
                    /*------------------------*/
                    /* Caricamento del modulo */
                    /*------------------------*/
                    $("#inv_003").submit()    ;
                }
            });
        },
        /*--------------------------------*/
        /* Scelta 'NO'                    */
        /*--------------------------------*/
        "NO" : function()
        {
            /*----------------------------*/
            /* Box di dialogo: close      */
            /*----------------------------*/
            $(this).dialog("close")       ;
        }
    });

    /*====================================*/
    /* Responsabile                       */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    rsp_des = $('#rsp_des').val()         ;

    /*====================================*/
    /* Codice prodotto                    */
    /*------------------------------------*/
    alf_pro = $('#alf_pro').val()         ;

    /*====================================*/
    /* Test su presenza responsabile      */
    /*------------------------------------*/
    if (!rsp_doc || rsp_doc == 0)
    {
        /*--------------------------------*/
        /* Disabilitazione prodotto       */
        /*--------------------------------*/
        $('#alf_pro').prop("disabled",true);
        /*--------------------------------*/
        /* Disabilitazione ubicazioni     */
        /*--------------------------------*/
        $('#acc_ub1').prop("disabled",true);
        $('#acc_ub2').prop("disabled",true);
        $('#acc_ub3').prop("disabled",true);
        $('#acc_ub4').prop("disabled",true);
        /*--------------------------------*/
        /* Focus su responsabile          */
        /*--------------------------------*/
        $('#rsp_doc').focus()             ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        /*--------------------------------*/
        /* Cella responsabile in rosso    */
        /*--------------------------------*/
        cel_css("cel_ute", "R")           ;
        cel_css("cel_pro", "-")           ;
    }
     else
    {
        /*--------------------------------*/
        /* Abilitazione prodotto          */
        /*--------------------------------*/
        $('#alf_pro').prop("enabled",true);
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_ute", "-")           ;
        cel_css("cel_pro", "+")           ;
        $('#alf_pro').focus()             ;
        $('#alf_pro').select()            ;
    }

    /*====================================*/
    /* Test su presenza codice prodotto   */
    /*------------------------------------*/
    if (!alf_pro || alf_pro == 0)
    {
        /*--------------------------------*/
        /* Disabilitazione ubicazioni     */
        /*--------------------------------*/
        $('#acc_ub1').prop("disabled",true);
        $('#acc_ub2').prop("disabled",true);
        $('#acc_ub3').prop("disabled",true);
        $('#acc_ub4').prop("disabled",true);
    }
     else
    {
        /*--------------------------------*/
        /* Abilitazione ubicazioni        */
        /*--------------------------------*/
        $('#acc_ub1').prop("enabled",true);
        $('#acc_ub2').prop("enabled",true);
        $('#acc_ub3').prop("enabled",true);
        $('#acc_ub4').prop("enabled",true);
        /*--------------------------------*/
        /* Cella responsabile in verde    */
        /*--------------------------------*/
        cel_css("cel_ute", "G")           ;
    }

    /*====================================*/
    /* Cambio Operatore                   */
    /*------------------------------------*/
    $('#rsp_doc').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_003").submit()            ;
    });          

    /*====================================*/
    /* Selezionato prodotto               */
    /*------------------------------------*/
    $('#alf_pro').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
    });          

    /*====================================*/
    /* Cambio prodotto                    */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_003").submit()            ;
    });          

    /*====================================*/
    /* Modifica ubicazione in chiave      */
    /*------------------------------------*/
    $('#cod_ubi').change(function()
    {
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var ubi = $('#cod_ubi').val()     ;
        var prm = 'cod_ubi=' + ubi        ;
        /*--------------------------------*/
        /* Lettura                        */
        /*--------------------------------*/
        $.ajax({ type: 'POST',
                  url: "./eleinvu0",
          contentType: "application/x-www-form-urlencoded",
                 data: prm,
              success: function(data)
              {
                /*------------------------*/
                /* Esito lettura          */
                /*------------------------*/
                let res = data.substring(0, 2);
                /*------------------------*/
                /* Messaggio di errore    */
                /*------------------------*/
                if (res != 'OK')
                {alert ('Ubicazione inesistente');}
            } 
        });    
    });          

    /*====================================*/
    /* Bottone nuovo record               */
    /*------------------------------------*/
    $('#new').click(function()
    {
        /*--------------------------------*/
        /* Codice ubicazione              */
        /*--------------------------------*/
        var cod_ubi = $('#cod_ubi').val();

        if (cod_ubi == '')
        {
            alert('Inserire un codice ubicazione');
            return                       ;
        };

        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WR"                ;
        /*--------------------------------*/
        /* Valori da tabella              */
        /*--------------------------------*/
        var num_pro = $('#num_pro').val();
        /*================================*/
        /* Parametri                      */
        /*--------------------------------*/
        const par = []                    ;
        const val = []                    ;
        /*--------------------------------*/
        par[0] = "tip_ope"                ;
        val[0] = tip_ope                  ;
        /*--------------------------------*/
        par[1] = "num_pro"                ;
        val[1] = num_pro                  ;
        /*--------------------------------*/
        par[2] = "cod_ubi"                ;
        val[2] = cod_ubi                  ;
        /*--------------------------------*/
        /* Assemblaggio                   */
        /*--------------------------------*/
        prm = ass_prm(par, val)           ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./eleinvw0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $("#inv_003").submit()    ;
            }
        });

    });

    /*====================================*/
    /* Bottone Non conformita'            */
    /*------------------------------------*/
    $('.ncf').click(function()
    {
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_rw2 = "#rw2_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Apertura / chiusura riga       */
        /*--------------------------------*/
        $(ide_rw2).toggle()               ;
        /*--------------------------------*/
        /* Focus                          */
        /*--------------------------------*/
        $(this).closest("tr").find('#nct').focus();
    });

    /*====================================*/
    /* Bottone Cancella                   */
    /*------------------------------------*/
    $('.del_btn').click(function()
    {
        /*--------------------------------*/
        /* Messaggio conferma operazione  */
        /*--------------------------------*/
        var res_msg = 'Vuoi veramente cancellare questa ubicazione?';
        /*--------------------------------*/
        /* Box di conferma operazione     */
        /*--------------------------------*/
        var riga = $(this).closest("tr").find('#pri').val();
        var my_d = $("#dialog-confirm").data('riga', riga);
        $('#dialog-confirm').html(res_msg);
        $('#dialog-confirm').dialog("option","title","ATTENZIONE !").dialog('open');
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
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#rw2_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var cod_ubi = $(ide_row).find('#ubi').val();
        var alf_pro = $(ide_row).find('#alf').val();
        var num_pro = $(ide_row).find('#pro').val();
        var qta_prs = $(ide_row).find('#qta').val();
        var qta_rlv = $(ide_row).find('#qtv').val();
        var ann_ncf = $(ide_row).find('#nct').val();
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
            $(ide_row).find('#qtv').prop("disabled",true);
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
            $(ide_row).find('#qtv').prop("disabled",false);
        }
        
        /*================================*/
        /* Parametri                      */
        /*--------------------------------*/
        const par = []                    ;
        const val = []                    ;
        /*--------------------------------*/
        par[0] = "rsp_doc"                ;
        val[0] = rsp_doc                  ;
        /*--------------------------------*/
        par[1] = "tip_ope"                ;
        val[1] = tip_ope                  ;
        /*--------------------------------*/
        par[2] = "cod_ubi"                ;
        val[2] = cod_ubi                  ;
        /*--------------------------------*/
        par[3] = "alf_pro"                ;
        val[3] = alf_pro                  ;
        /*--------------------------------*/
        par[4] = "num_pro"                ;
        val[4] = num_pro                  ;
        /*--------------------------------*/
        par[5] = "qta_prs"                ;
        val[5] = qta_prs                  ;
        /*--------------------------------*/
        par[6] = "qta_rlv"                ;
        val[6] = qta_rlv                  ;
        /*--------------------------------*/
        par[7] = "flg_spn"                ;
        val[7] = flg_spn                  ;
        /*--------------------------------*/
        par[8] = "ann_ncf"                ;
        val[8] = ann_ncf                  ;
        /*--------------------------------*/
        /* Assemblaggio                   */
        /*--------------------------------*/
        prm = ass_prm(par, val)           ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./eleinvw0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $("#inv_003").submit()    ;
            }
        });
    });          


    /*====================================*/
    /* Modifica ubicazione in riga        */
    /*------------------------------------*/
    $('.acc_ubi').change(function()
    {
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var ubi = $(ide_row).find('#ubi').val();
        var prm = 'cod_ubi=' + ubi        ;
        /*--------------------------------*/
        /* Lettura                        */
        /*--------------------------------*/
        $.ajax({ type: 'POST',
                  url: "./eleinvu0",
          contentType: "application/x-www-form-urlencoded",
                 data: prm,
              success: function(data)
              {
                /*------------------------*/
                /* Esito lettura          */
                /*------------------------*/
                let res = data.substring(0, 2);
                /*------------------------*/
                /* Messaggio di errore    */
                /*------------------------*/
                if (res != 'OK')
                {alert ('Ubicazione inesistente');}
            } 
        });    
    });          

    /*====================================*/
    /* Modifica box di spunta, quantita'  */
    /* o ubicazione                       */
    /*------------------------------------*/
    $('input[type="checkbox"],.numbersOnly,.acc_ubi').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WI"                ;
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var cod_ubi = $(ide_row).find('#ubi').val();
        var alf_pro = $(ide_row).find('#alf').val();
        var num_pro = $(ide_row).find('#pro').val();
        var qta_prs = $(ide_row).find('#qta').val();
        var qta_rlv = $(ide_row).find('#qtv').val();
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
            $(ide_row).find('#qtv').prop("disabled",true);
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
            $(ide_row).find('#qtv').prop("disabled",false);
        }

        /*--------------------------------*/
        /* Messaggio di quantita' a zero  */
        /*--------------------------------*/
        if (qta_rlv == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*================================*/
        /* Parametri                      */
        /*--------------------------------*/
        const par = []                    ;
        const val = []                    ;
        /*--------------------------------*/
        par[0] = "rsp_doc"                ;
        val[0] = rsp_doc                  ;
        /*--------------------------------*/
        par[1] = "tip_ope"                ;
        val[1] = tip_ope                  ;
        /*--------------------------------*/
        par[2] = "cod_ubi"                ;
        val[2] = cod_ubi                  ;
        /*--------------------------------*/
        par[3] = "alf_pro"                ;
        val[3] = alf_pro                  ;
        /*--------------------------------*/
        par[4] = "num_pro"                ;
        val[4] = num_pro                  ;
        /*--------------------------------*/
        par[5] = "qta_prs"                ;
        val[5] = qta_prs                  ;
        /*--------------------------------*/
        par[6] = "qta_rlv"                ;
        val[6] = qta_rlv                  ;
        /*--------------------------------*/
        par[7] = "flg_spn"                ;
        val[7] = flg_spn                  ;
        /*--------------------------------*/
        /* Assemblaggio                   */
        /*--------------------------------*/
        prm = ass_prm(par, val)           ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./eleinvw0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $("#inv_003").submit()    ;
            }
        });
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

/*========================================*/
/* Funzione per assemblaggio parametri    */
/*----------------------------------------*/
function ass_prm(nom, val)
{
    /*------------------------------------*/
    /* Normalizzazioni                    */
    /*------------------------------------*/
    let inx = 0                           ;
    let par = ''                          ;
    /*------------------------------------*/
    /* Ciclo                              */
    /*------------------------------------*/
    while (inx < nom.length)
    {
        /*--------------------------------*/
        /* Assemblaggio                   */
        /*--------------------------------*/
        par = par 
            + nom[inx]
            + "=" 
            + val[inx]
            + "&"                         ;
        /*--------------------------------*/
        /* Incremento indice              */
        /*--------------------------------*/
        inx++;
    }        
    /*------------------------------------*/
    /* Valore in uscita                   */
    /*------------------------------------*/
    return par                            ;
/*----------------------------------------*/
/* Funzione richiesta inserimento - fine  */
/*========================================*/
}

