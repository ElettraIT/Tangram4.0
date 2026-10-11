/*========================================*/
/* eleubi02 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 22/10/2024           */
/*========================================*/

/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
mod_frm = "#ubi_002"                      ;
url_pro = "./elepro01"                    ;

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione prodotti per ubicazione */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal   : true,
        title   : 'Scheda prodotto',
        width   : 450,
        height  : 450,
    });

    /*====================================*/
    /* Campi form                         */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    cod_ubi = $('#cod_ubi').val()         ;

    /*====================================*/
    /* Codice ubicazione                  */
    /*------------------------------------*/
    cel_css("cel_ubi", "+")               ;
    $('#cod_ubi').focus()                 ;
    $('#cod_ubi').select()                ;

    /*====================================*/
    /* Test su presenza responsabile      */
    /*------------------------------------*/
    if (!rsp_doc)
    {
        /*--------------------------------*/
        /* Test iniziale su indirizzo IP  */
        /* individuare l'operatore        */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url : "./getip.pl",
            contentType: "application/x-www-form-urlencoded",
            /*----------------------------*/
            /* Se indirizzo determinato   */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Determinazione         */
                /*------------------------*/
                var ipx_val = data.trim() ;
                let_ipx(ipx_val)          ;
            }
        });
    };

    /*====================================*/
    /* Selezionato ubicazione             */
    /*------------------------------------*/
    $('#cod_ubi').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
    });          

    /*====================================*/
    /* Cambio ubicazione                  */
    /*------------------------------------*/
    $('#cod_ubi').change(function()
    {
        /*--------------------------------*/
        /* Pulizia del codice ubicazione  */
        /*--------------------------------*/
        var pul_ubi = $('#cod_ubi').val() ;
            pul_ubi = pul_str (pul_ubi)   ;
        $('#cod_ubi').val(pul_ubi)        ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Click su codice ubicazione         */
    /*------------------------------------*/
    $('#ubi td').click(function()
    {
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var num_cln = $(this).parent().children().index($(this));
        var alf_pro = $(this).closest("tr").find('#alf').val();

        /*================================*/
        /* Se colonna del codice prodotto */
        /*--------------------------------*/
        if (num_cln == 0)
        {
            /*----------------------------*/
            /* Codice prodotto            */
            /*----------------------------*/
            var alf_pro = $(this).closest("tr").find('#alf').val();
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: "./eleodsg0",
                contentType: "application/x-www-form-urlencoded",
                data: "alf_pro=" + alf_pro,
                success: function (data)
                {
                      $("#dialog").html(data);
                      $("#dialog").dialog("option","title","Prodotto : " + alf_pro).dialog('open');
                }
            });
        }
        /*================================*/
        /* Evento sul click della colonna */
        /* descrizione                    */
        /*--------------------------------*/
        if (num_cln == 1) {
            /*----------------------------*/
            /* Codice prodotto            */
            /*----------------------------*/
            var rsp_doc = $('#rsp_doc').val();
            var alf_pro = $(this).closest("tr").find('#alf').val();
            /*----------------------------*/
            /* Parametri da passare       */
            /*----------------------------*/
            var par_frm = {
                "alf_pro": alf_pro,
                "rsp_doc": rsp_doc
            };
            /*----------------------------*/
            /* URL dell'endpoint per il   */
            /* form POST                  */
            /*----------------------------*/
            var url = url_pro             ;
            /*----------------------------*/
            /* Apri dialog con iframe     */
            /*----------------------------*/
            dlg_ifr(1100, 750, alf_pro)   ;
            /*----------------------------*/
            /* Invia form dinamico        */
            /*----------------------------*/
            inv_frm(url, par_frm, "ifr_pro");
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
/* Funzione di pulizia stringa            */
/*                                        */
/* N.B.: usata per togliere caratteri non */
/*       alfabetici o numerici            */
/*----------------------------------------*/
function pul_str(inp_str)
{
    return inp_str.replace(/[^a-zA-Z0-9]/g, '');
};    
    
/*========================================*/
/* Funzione per creare e gestire un       */
/* Dialog con iframe                      */
/*----------------------------------------*/
function dlg_ifr( wdt_dlg, hgt_dlg, pro_dlg)
{
    /*------------------------------------*/
    /* Creazione del contenitore del      */
    /* dialog con iframe                  */
    /*------------------------------------*/
    var dlg_prm = '<iframe '
                + 'id="ifr_pro" '
                + 'name="ifr_pro" '
                + 'style="border:none;" '
                + 'width="100%" '
                + 'height="100%">'
                + '</iframe>'             ;
    var dlg_div = $('<div></div>').append(dlg_prm);

    /*------------------------------------*/
    /* Impostazione del dialog con iframe */
    /*------------------------------------*/
    dlg_div.dialog({
        title    : "Dettagli Prodotto " + pro_dlg,
        autoOpen : true,
        width    : wdt_dlg || 800,  
        height   : hgt_dlg || 600, 
        modal    : true,
        resizable: false,
        close    : function()
        {
            /*----------------------------*/
            /* Distruzione del dialog al  */
            /* chiusura                   */
            /*----------------------------*/
            $(this).dialog('destroy').remove();
        }
    });
}

/*========================================*/
/* Funzione per creare e inviare un form  */
/* dinamico in POST                       */
/*----------------------------------------*/
function inv_frm (url, prm, frm)
{
    /*------------------------------------*/
    /* Creazione del form dinamico        */
    /*------------------------------------*/
    var form = $('<form>', {
        "method": "POST",
        "action": url,
        "target": frm
    });

    /*------------------------------------*/
    /* Aggiunta dei parametri al form     */
    /*------------------------------------*/
    $.each(prm, function(name, value) {
        form.append($('<input>', {
            "type": "hidden",
            "name": name,
            "value": value
        }));
    });

    /*------------------------------------*/
    /* Aggiunta del form al body e invio  */
    /*------------------------------------*/
    $('body').append(form)                ;
    form.submit()                         ;
    /*------------------------------------*/
    /* Rimozione del form dopo l'invio    */
    /*------------------------------------*/
    form.remove()                         ;
}

