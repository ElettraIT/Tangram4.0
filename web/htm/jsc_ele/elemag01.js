/*========================================*/
/* elemag01 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 16/02/2024           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{

    /*====================================*/
    /* Variabili globali                  */
    /*------------------------------------*/
    /* Livello operativo                  */
    /*                                    */
    /* - '0' : Login utente epassword     */
    /* - '1' : Accettazione protocolli    */
    /* - '2' : Accettazione prodotti      */
    /*------------------------------------*/
    liv_ope = $('#liv_ope').val()         ;

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
    /* Deviazione in funzione del livello */
    /*------------------------------------*/
    if (liv_ope == 0 || !liv_ope)
    {
        /*--------------------------------*/
        /* Livello '0'                    */
        /*--------------------------------*/
        liv_ope = 0                       ;
        liv_000 ()                        ;
    }
     else if (liv_ope == 1)
    {
        /*--------------------------------*/
        /* Livello '1'                    */
        /*--------------------------------*/
        liv_001 ()                       ;
    }
     else
    {
        /*--------------------------------*/
        /* Livello '2'                    */
        /*--------------------------------*/
        liv_002 ()                       ;
    }

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
/* Funzione di gestione livello '0'       */
/*                                        */
/* Login                                  */
/*----------------------------------------*/
function liv_000 ()
{
    /*====================================*/
    /* Cella di accettazione in rosso     */
    /*------------------------------------*/
    cel_css("cel_ute", "R")               ;

    /*====================================*/
    /* Focus su utente                    */
    /*------------------------------------*/
    if ($('#cod_ute').val() != '')
    {$('#pwd_ute').focus()}
     else
    {$('#cod_ute').focus()}               ;

    /*====================================*/
    /* Cambio Utente                      */
    /*------------------------------------*/
    $('#cod_ute').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_000").submit()            ;
    });          

    /*====================================*/
    /* Cambio Password                    */
    /*------------------------------------*/
    $('#pwd_ute').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_000").submit()            ;
    });          

/*----------------------------------------*/
/* Fine funzione livello '0'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '1'       */
/*                                        */
/* Input prodotti                         */
/*----------------------------------------*/
function liv_001 ()
{

    /*====================================*/
    /* Focus su classe                    */
    /*------------------------------------*/
    $('#cod_cla').focus()                 ;
    $('#cod_cla').select()                ;

    /*====================================*/
    /* Definizione radio buttons          */
    /*------------------------------------*/
    $('.snx_rad').checkboxradio()         ;

    /*====================================*/
    /* Highlight classe                   */
    /*------------------------------------*/
    $('#cod_cla').click(function()
    {
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        cel_css("cel_num", "+")           ;
    });

    /*====================================*/
    /* Cambio si/no giacenza              */
    /*------------------------------------*/
    $('.snx').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_000").submit()            ;
    });          

    /*====================================*/
    /* Cambio prodotto                    */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_000").submit()            ;
    });          

    /*====================================*/
    /* Cambio classe                      */
    /*------------------------------------*/
    $('#cod_cla').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazione prodotto       */
        /*--------------------------------*/
        $("#alf_pro").val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_000").submit()            ;
    });          

    /*====================================*/
    /* Cambio gruppo                      */
    /*------------------------------------*/
    $('#cod_gru').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_000").submit()            ;
    });          

    /*====================================*/
    /* Cambio sottogruppo                 */
    /*------------------------------------*/
    $('#cod_sgr').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_000").submit()            ;
    });          

    /*====================================*/
    /* Bottone conferma                   */
    /*------------------------------------*/
    $('#but_fip').click(function()
    {
    
    
        var tip_ord = $('#tip_ord').val() ;
        
        
        
        /*--------------------------------*/
        /* Deviazione in funzione del     */
        /* tipo selezione                 */
        /*--------------------------------*/
        switch(tip_ord)
        {
            /*============================*/
            /* Nessuna selezione          */
            /*----------------------------*/
            case '':
                /*------------------------*/
                /* Messaggio              */
                /*------------------------*/
                alert ("Manca il Tipo selezione!")
                /*------------------------*/
                /* Oltre                  */
                /*------------------------*/
                break                     ;
            /*============================*/
            /* Prodotto                   */
            /*----------------------------*/
            case 'P':
                /*------------------------*/
                /* Test su prodotto       */
                /*------------------------*/
                var alf_pro = $('#alf_pro').val() ;
                /*------------------------*/
                /* Se a spazi             */
                /*------------------------*/
                if (alf_pro == '')
                {
                    /*--------------------*/
                    /* Avviso             */
                    /*--------------------*/
                    alert ("Manca il Codice prodotto!")}
                 else
                {
                    /*--------------------*/
                    /* Livello successivo */
                    /*--------------------*/
                    $('#liv_ope').val('2');
                    /*--------------------*/
                    /* Caricamento modulo */
                    /*--------------------*/
                    $("#inv_000").submit();
                }
                /*------------------------*/
                /* Oltre                  */
                /*------------------------*/
                break                     ;
            /*============================*/
            /* Ubicazione                 */
            /*----------------------------*/
            case 'U':
                /*------------------------*/
                /* Test su ubicazione     */
                /*------------------------*/
                var cod_ubi = $('#cod_ubi').val() ;
                /*------------------------*/
                /* Se a spazi             */
                /*------------------------*/
                if (cod_ubi == '')
                {
                    /*--------------------*/
                    /* Avviso             */
                    /*--------------------*/
                    alert ("Manca il Codice ubicazione!")
                    /*--------------------*/
                    /* Livello successivo */
                    /*--------------------*/
                    $('#liv_ope').val('2');
                    /*--------------------*/
                    /* Caricamento modulo */
                    /*--------------------*/
                    $("#inv_000").submit();
                }
                 else
                {
                    /*--------------------*/
                    /* Livello successivo */
                    /*--------------------*/
                    $('#liv_ope').val('2');
                    /*--------------------*/
                    /* Caricamento modulo */
                    /*--------------------*/
                    $("#inv_000").submit();
                }
                /*------------------------*/
                /* Oltre                  */
                /*------------------------*/
                break                     ;
            /*============================*/
            /* Classe                     */
            /*----------------------------*/
            case 'C':
                /*------------------------*/
                /* Test su ubicazione     */
                /*------------------------*/
                var cod_cla = $('#cod_cla').val() ;
                /*------------------------*/
                /* Se a spazi             */
                /*------------------------*/
                if (cod_cla == 0)
                {
                    /*--------------------*/
                    /* Avviso             */
                    /*--------------------*/
                    alert ("Manca il Codice classe!")}
                 else
                {
                    /*--------------------*/
                    /* Livello successivo */
                    /*--------------------*/
                    $('#liv_ope').val('2');
                    /*--------------------*/
                    /* Caricamento modulo */
                    /*--------------------*/
                    $("#inv_000").submit();
                }
                /*------------------------*/
                /* Oltre                  */
                /*------------------------*/
                break                     ;
        };
        
    /*------------------------------------*/
    /* Bottone conferma - fine            */
    /*====================================*/
    });

    /*====================================*/
    /* Bottone annullamento               */
    /*------------------------------------*/
    $('#but_fsu').click(function()
    {
        /*--------------------------------*/
        /* A livello precedente           */
        /*--------------------------------*/
        $('#liv_ope').val('1')            ;
        /*--------------------------------*/
        /* Normalizzazione classe         */
        /*--------------------------------*/
        $('#cod_cla').val('')             ;
        /*--------------------------------*/
        /* Normalizzazione gruppo         */
        /*--------------------------------*/
        $('#cod_gru').val('')             ;
        /*--------------------------------*/
        /* Normalizzazione sottogruppo    */
        /*--------------------------------*/
        $('#cod_sgr').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_000").submit()            ;
    });

    /*====================================*/
    /* Bottone rientro menu               */
    /*------------------------------------*/
    $('#but_mnu').click(function()
    {
        /*--------------------------------*/
        /* A livello precedente           */
        /*--------------------------------*/
        $(location).prop('href', '../ele2.html')
    });

/*----------------------------------------*/
/* Fine funzione livello '1'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '2'       */
/*                                        */
/* Input Spunte                           */
/*----------------------------------------*/
function liv_002 ()
{
    /*====================================*/
    /* Definizione tabella                */
    /*------------------------------------*/
    let table = new DataTable('#mxport')  ;

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* conferma spunta totale             */
    /*------------------------------------*/
    $('#dialog-confirm').dialog({
      autoOpen: false,
      resizable: false,
      height: "auto",
      width: 400,
      modal: true,
      buttons: {
        "Vuoi spuntare tutti le righe rimanenti?": function() {
          $( this ).dialog( "close" );
        },
        Cancel: function() {
          $( this ).dialog( "close" );
        }
      },

        /*--------------------------------*/
        /* Default 'NO'                   */
        /* ___ NON FUNZIONA ___           */
        /*--------------------------------*/
      open: function() {
          $('#dialog-confirm.ui-dialog-buttonpane button:eq(1)').focus();
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
            /* Responsabile               */
            /*----------------------------*/
            var rsp_doc = $('#rsp_doc').val();
            /*----------------------------*/
            /* Tipo operazione            */
            /*----------------------------*/
            var tip_ope = "WG"           ;
            /*----------------------------*/
            /* Numero distinta            */
            /*----------------------------*/
            var num_dst = $('#num_dst').val();
            /*----------------------------*/
            /* Parametri                  */
            /*----------------------------*/
            var prm_01 = "rsp_doc=" + rsp_doc ;
            var prm_02 = "tip_ope=" + tip_ope ;
            var prm_03 = "num_dst=" + num_dst ;
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            var prm = prm_01 + "&" +
                      prm_02 + "&" +
                      prm_03              ;
            /*----------------------------*/
            /* Aggiornamento Tangram      */
            /*----------------------------*/
            $.ajax({
                /*------------------------*/
                /* Richiamo modulo        */
                /*------------------------*/
                type: "POST",
                url: "./eleodsw0",
                contentType: "application/x-www-form-urlencoded",
                data: prm,
                /*------------------------*/
                /* Se andato a buon fine  */
                /*------------------------*/
                success: function (data)
                {
                    /*--------------------*/
                    /* Messaggi           */
                    /*--------------------*/
                    $('#msg').html(data)  ;
                    /*--------------------*/
                    /* Ricarica modulo    */
                    /*--------------------*/
                    $("#inv_000").submit();
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
    /* Focus su prodotto                  */
    /*------------------------------------*/
    $('#alf_pro').focus()                 ;
    $('#alf_pro').select()                ;

    /*====================================*/
    /* Cambio prodotto                    */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_000").submit()            ;
    });          

    /*====================================*/
    /* Cambio Prodotto da verificare      */
    /*------------------------------------*/

    /*====================================*/
    /* Controllo click su elementi riga   */
    /*------------------------------------*/
    table.on('click', 'td', function ()
    {
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var num_cln = $(this).parent().children().index($(this));
        /*--------------------------------*/
        /* Se colonna della ubicazione    */
        /*--------------------------------*/
        if (num_cln == 17)
        {
            /*----------------------------*/
            /* Codice prodotto            */
            /*----------------------------*/
            var cod = $(ide_row).find('#alf').val();
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: "./eleinv11",
                contentType: "application/x-www-form-urlencoded",
                data: "alf_pro=" + cod,
                success: function (data)
                {
                      $("#dialog").html(data);
                      $("#dialog").dialog("option","title","Prodotto : " + cod).dialog('open');
                }
            });
        }
    /*------------------------------------*/
    /* Controllo click su elementi riga   */
    /*                                    */
    /* Fine                               */
    /*====================================*/
    });

    /*====================================*/
    /* Highlight prodotto                 */
    /*------------------------------------*/
    $('.numbersOnly').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        cel_css("cel_num", "+")           ;
    });

    /*====================================*/
    /* Modifica box di spunta o quantita' */
    /*------------------------------------*/
    table.on('change', '.chk_spn,.numbersOnly', function ()
//    $('.chk_spn,.numbersOnly').change(function()
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
        if ($(ide_row).find('.chk_spn').is(":checked"))
//        if ($(ide_row).find('input[type="checkbox"]').is(":checked"))
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
        /* DEBUG                          */
        /*--------------------------------*/
//        console.log(prm);
        
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
                /* DEBUG                  */
                /*------------------------*/
//                console.log(data);
        
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
//                $("#inv_000").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Modifica box spunta supervisione   */
    /*------------------------------------*/
    table.on('change', '.chk_spv', function ()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WS"                ;
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var cod_ubi = $(ide_row).find('#ubi').val();
        var alf_pro = $(ide_row).find('#alf').val();
        var num_pro = $(ide_row).find('#pro').val();
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('.chk_spv').is(":checked"))
        {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spv = 'S'             ;
        }
         else
        {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spv = 'N'             ;
        }

        /*================================*/
        /* Parametri                      */
        /*--------------------------------*/
        const par = []                    ;
        const val = []                    ;
        /*--------------------------------*/
        par[0] = "tip_ope"                ;
        val[0] = tip_ope                  ;
        /*--------------------------------*/
        par[1] = "cod_ubi"                ;
        val[1] = cod_ubi                  ;
        /*--------------------------------*/
        par[2] = "alf_pro"                ;
        val[2] = alf_pro                  ;
        /*--------------------------------*/
        par[3] = "num_pro"                ;
        val[3] = num_pro                  ;
        /*--------------------------------*/
        par[4] = "flg_spv"                ;
        val[4] = flg_spv                  ;
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
                /* Ricarica modulo        */
                /*------------------------*/
    //            $("#inv_000").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Bottone fine spunta ubicazione     */
    /*------------------------------------*/
    $('#but_fsu').click(function()
    {
        /*--------------------------------*/
        /* A livello precedente           */
        /*--------------------------------*/
        $('#liv_ope').val('1')            ;
        /*--------------------------------*/
        /* Normalizzazione ubicazione     */
        /*--------------------------------*/
        $('#cod_ubi').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_000").submit()            ;
    });
    
    /*====================================*/
    /* Bottone refresh                    */
    /*------------------------------------*/
    $('#but_ref').click(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#inv_000").submit()            ;
    });
    
    /*====================================*/
    /* Bottone completamento spunte       */
    /*------------------------------------*/
    $('#but_gen').click(function()
    {
        /*--------------------------------*/
        /* ATTUALMENTE INIBITO            */
        /*--------------------------------*/
        alert("Funzione non disponibile");
        return;

        /*--------------------------------*/
        /* Calcolo righe da spuntare      */
        /*--------------------------------*/
        var rig = 0                       ;
        var spt = 0                       ;
        var nsp = 0                       ;
        $("#rig tr").each(function()
        {
            var unc = $(this).find('input[type="checkbox"]:not(:checked)');
            if (unc.length)
            {nsp++} else {spt++}          ;
            rig++                         ;
        });                  
        var dsp = rig - spt               ;
        /*--------------------------------*/
        /* Messaggio conferma operazione  */
        /*--------------------------------*/
        var res_msg = ''                  ;
        
        if (dsp == 1)
        {
            res_msg = '<h1>Vuoi veramente spuntare la riga rimanente?</h1>';
        }
         else
        {
            res_msg = '<h1>Vuoi veramente spuntare le ' + dsp + ' righe rimanenti?</h1>';
        }
        /*--------------------------------*/
        /* Box di conferma operazione     */
        /*--------------------------------*/
        $('#dialog-confirm').html(res_msg);
        $('#dialog-confirm').dialog("option","title","ATTENZIONE !").dialog('open');
    });

    /*====================================*/
    /* Bottone Non conformita'            */
    /*------------------------------------*/

/*----------------------------------------*/
/* Fine funzione livello '2'              */
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


