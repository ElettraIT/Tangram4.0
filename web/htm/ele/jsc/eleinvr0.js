/*========================================*/
/* eleinvr0 - Javascript - by WIP         */
/*                                        */
/* Color picker per responsabili          */
/*                                        */
/* Elettra : Controllo inventario         */
/* -------------------------------------- */
/* Aggiornamento   : 19/11/2024           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
mod_frm = "#col_rsp"                      ;
url_wrt = "../ele/eleinvw1"               ;

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{

    /*====================================*/
    /* Chiusura preliminare box codice    */
    /* colore                             */
    /*------------------------------------*/
    $('#cod_col').hide()                  ;

    /*====================================*/
    /* Determinazione responsabile sele-  */
    /* zionato                            */
    /*------------------------------------*/
    $('#rsp td').on('click', function()
    {
        /*--------------------------------*/
        /* Codice selezionato             */
        /*--------------------------------*/
        var idx = $(this).attr('id')      ;
        /*--------------------------------*/
        /* Descrizione da box selezionato */
        /*--------------------------------*/
        var des_rsp = $(this).text();
            des_rsp = des_rsp.trim()      ;
        /*--------------------------------*/
        /* Assegnazione valori letti      */
        /*--------------------------------*/
        $("#swatch").html("<br><h5>" + des_rsp + "</h5>");
        $('#cod_rsp').val(idx);
        /*--------------------------------*/
        /* Evidenziazione cella selezio-  */
        /* nata                           */
        /*--------------------------------*/
        cel_brd(idx, "+")           ;
    });

    /*====================================*/
    /* Bottone conferma cambio colore     */
    /*------------------------------------*/
    $('#but_fip').click(function()
    {
        /*--------------------------------*/
        /* Codice responsabile            */
        /*--------------------------------*/
        var rsp_doc = $('#cod_rsp').val() ;
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "CR"                ;
        /*--------------------------------*/
        /* Codice colore                  */
        /*--------------------------------*/
        var cod_col = $("#cod_col").text();

        /*================================*/
        /* Test su codice responsabile    */
        /*--------------------------------*/
        if (rsp_doc == '')
        {
            /*----------------------------*/
            /* Se non selezionato codice  */
            /* responsabile               */
            /*----------------------------*/
            alert("Selezionare un responsabile!");
            return                        ;
        };

        /*================================*/
        /* Test su codice colore          */
        /*--------------------------------*/
        if (cod_col == '')
        {
            /*----------------------------*/
            /* Se non selezionato codice  */
            /* colore                     */
            /*----------------------------*/
            alert("Selezionare un colore!");
            return                        ;
        };

        /*================================*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "cod_col": cod_col
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;

        /*================================*/
        /* Messaggio per la richiesta di  */
        /* conferma                       */
        /*--------------------------------*/
        var rsp_des = $('#swatch').text() ;
        var cnf_msg = 'Vuoi cambiare il colore'
                    + ' a '
                    + rsp_des
                    + ' ?';
        var cnf_wdt = 400                 ;

        /*================================*/
        /* Richiesta di conferma spunta   */
        /*--------------------------------*/
        cnf_dlg(cnf_msg, cnf_wdt).done(function()
        {
        /*================================*/
        /* Scelta 'SI'                    */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url : url_wrt,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $(mod_frm).submit()       ;
            }
        });

        /*================================*/
        /* Scelta 'NO'                    */
        /*--------------------------------*/
        }).fail(function()
        {
            /*----------------------------*/
            /* Nessuna azione             */
            /*----------------------------*/
        });

    /*------------------------------------*/
    /* Bottone conferma cambio colore     */
    /*                                    */
    /* Fine                               */
    /*====================================*/
    });

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/
});

/*========================================*/
/* FUNZIONI                               */
/*========================================*/

/*========================================*/
/* Funzione generica di switch bordo      */
/* cella                                  */
/*----------------------------------------*/
function cel_brd (field, swc)
{
    /*------------------------------------*/
    /* Normalizzazione bordo celle        */
    /*------------------------------------*/
    $('table#rsp')
    .find('.nom_rsp')
    .css({"border": "1px solid"});
    /*------------------------------------*/
    /* Evidenziazione bordo della cella   */
    /* selezionata                        */
    /*------------------------------------*/
    $("#"+field).css("border",
                             "8px solid")   ;

/*----------------------------------------*/
/* Fine funzione                          */
/*========================================*/
}

/*========================================*/
/* Funzione di richiesta conferma spunta  */
/*----------------------------------------*/
function cnf_dlg(msg, wdt)
{
    /*------------------------------------*/
    /* Messaggio per il box               */
    /*------------------------------------*/
    var dialog = $("<div><h1>"+msg+"</h1></div>")  ;
    /*------------------------------------*/
    /* Variabile per si/no                */
    /*------------------------------------*/
    var def = $.Deferred()                ;
    /*------------------------------------*/
    /* Creazione finestra di dialogo      */
    /*------------------------------------*/
    $(dialog).dialog(
    {
        /*--------------------------------*/
        /* Parametri box                  */
        /*--------------------------------*/
        resizable: false,
        autoOpen : true,
        title    : "ATTENZIONE !",
        height   : "auto",
        width    : wdt,
        modal    : true,
        buttons:
        {
            'si': function()
            {
                def.resolve()             ;
                $(dialog).dialog( "close" );
            },
            'NO': function()
            {
                def.reject()              ;
                $(dialog).dialog( "close" );
            }
        },
        /*--------------------------------*/
        /* Bottone di default (NO)        */
        /*--------------------------------*/
        open: function()
        {
            $(this).siblings('.ui-dialog-buttonpane').find('button:eq(1)').focus(); 
        }
        
    });
    /*------------------------------------*/
    /* Variabile in uscita                */
    /*------------------------------------*/
    return def.promise()                  ;
/*----------------------------------------*/
/* Fine funzione                          */
/*========================================*/
}

/*========================================*/
/* Funzione Color Picker                  */
/*                                        */
/* ___ MIGLIORARE QUESTE FUNZIONI ___     */
/*----------------------------------------*/
$(function()
{
    /*------------------------------------*/
    /* Trattamento colore RGB             */
    /*------------------------------------*/
    function hexFromRGB(r, g, b)
    {
        /*--------------------------------*/
        /* Normalizzazione colori         */
        /*--------------------------------*/
        var hex = [
            r.toString(16),
            g.toString(16),
            b.toString(16)
        ];
    /*------------------------------------*/
    /* Lettura da selettore               */
    /*------------------------------------*/
    $.each( hex, function( nr, val )
    {
        if ( val.length === 1 )
            {
                hex[ nr ] = "0" + val     ;
            }
        });
    /*------------------------------------*/
    /* Valore in uscita                   */
    /*------------------------------------*/
    return hex.join( "" ).toUpperCase()   ;

/*----------------------------------------*/
/* Fine funzione                          */
/*========================================*/
}

/*========================================*/
/* Funzione di refresh tavolozza          */
/*----------------------------------------*/
function refreshSwatch()
{
    /*------------------------------------*/
    /* Recupero valori da selettori       */
    /*------------------------------------*/
    var red = $( "#red" ).slider( "value" ),
      green = $( "#green" ).slider( "value" ),
       blue = $( "#blue" ).slider( "value" ),
        hex = hexFromRGB( red, green, blue );
    
    /*------------------------------------*/
    /* Aggiornamento colore tavolozza     */
    /*------------------------------------*/
    $("#swatch").css( "background-color", "#" + hex );
  
    /*------------------------------------*/
    /* Assegnazione codice esadecimale    */
    /* colore                             */
    /*------------------------------------*/
    $( "#cod_col" ).html(hex)             ;

/*----------------------------------------*/
/* Fine funzione                          */
/*========================================*/
}

/*========================================*/
/* Definizione degli slider               */
/*----------------------------------------*/
    $( "#red, #green, #blue" ).slider({
      orientation: "horizontal",
      range : "min",
      max   : 255,
      value : 127,
      slide : refreshSwatch,
      change: refreshSwatch
    });
    $( "#red" ).slider( "value", 255 );
    $( "#green" ).slider( "value", 140 );
    $( "#blue" ).slider( "value", 60 );
  } );
