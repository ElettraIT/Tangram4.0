/*========================================*/
/* eleinvr1 - Javascript - by WIP         */
/*                                        */
/* Simple color picker per responsabili   */
/*                                        */
/* Elettra : Controllo inventario         */
/* -------------------------------------- */
/* Aggiornamento   : 24/11/2024           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
mod_frm = "#col_rsp"                      ;
url_wrt = "../ele/eleinvw1"               ;
ctr_ele = 0                               ;        

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Controllo elementi senza colore    */
    /*------------------------------------*/
    $("#rsp td").each(function()
    {
        /*--------------------------------*/
        /* Rilevazione colore per ciascun */
        /* elemento                       */
        /*--------------------------------*/
        var color = $(this).css( "background-color" );        
        /*--------------------------------*/
        /* Conteggio                      */
        /*--------------------------------*/
        if (color === 'rgba(0, 0, 0, 0)')
        {ctr_ele++}
        /*--------------------------------*/
        /* Test se rilevati elementi      */
        /* chi                            */
        /*--------------------------------*/
        if (ctr_ele === 1)
        {
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            var msg = '<h2>' + ctr_ele
                    + ' Elemento'
                    + ' senza sfondo</h2>';
            $('#ceb').html(msg)           ;
        }
        else if (ctr_ele > 0)
        {
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            var msg = '<h2>' + ctr_ele
                    + ' Elementi'
                    + ' senza sfondo</h2>';
            $('#ceb').html(msg)           ;
        }
    });                  

    /*====================================*/
    /* Controlli iniziali                 */
    /*------------------------------------*/
    if (!$('#cod_rsp').val() && !$('#cod_col').val())
    {
        $('#des_rsp').html("<h3> <--- Scegli un colore ed un responsabile</h3>");
    }

    /*====================================*/
    /* Simple Color Picker                */
    /*------------------------------------*/
    $('#scp').simpleColor({
        boxHeight : 80,
        cellWidth : 30,
        cellHeight: 20,
        chooserCSS: { 'border': '1px solid #660033' },
        displayCSS: { 'border': '1px solid red',
                      'border-radius': '4px'
                    },
        displayColorCode: true,
        livePreview: true,
        onSelect: function(hex, element)
        {
            /*----------------------------*/
            /* Assegnazione codice        */
            /* esadecimale colore         */
            /*----------------------------*/
            $("#cod_col").html(hex)       ;
            $("#box_rsp").css( "background-color", "#" + hex );
            /*----------------------------*/
            /* Test se presente codice    */
            /* responsabile               */
            /*----------------------------*/
            if (!$('#cod_rsp').val())
            {
                $('#des_rsp').html("<h3> Scegli un responsabile</h3>");
            }
        }
    });

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
        $('#des_rsp').html("<h5>" + des_rsp + "</h5>");
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
        var rsp_des = $('#des_rsp').text() ;
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

