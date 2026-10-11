/*========================================*/
/* elebol01 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 10/10/2024           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
mod_frm = "#bol_001"                      ;

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Numero documento                   */
    /*------------------------------------*/
    num_bol = $('#num_bol').val()         ;
    /*------------------------------------*/
    /* Responsabile                       */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    rsp_des = $('#rsp_doc option:selected').text();

    /*====================================*/
    /* Test su presenza responsabile      */
    /*------------------------------------*/
    if (!rsp_doc || rsp_doc == 0)
    {
        /*--------------------------------*/
        /* Se responsabile da impostare:  */
        /* disabilitazioni                */
        /*--------------------------------*/
        $('#num_bol').prop("disabled",true);
        $('#acc_spn').prop("disabled",true);
        /*--------------------------------*/
        /* Focus su responsabile          */
        /*--------------------------------*/
        $('#rsp_doc').focus()             ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_ute", "R")           ;
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
    }
     else
    {
        /*--------------------------------*/
        /* Se responsabile impostato:     */
        /* abilitazioni                   */
        /*--------------------------------*/
        $('#num_bol').prop("enabled",true);
        $('#acc_spn').prop("enabled",true);
        cel_css("cel_num", "+")           ;
        $('#num_bol').focus()             ;
    }

    /*====================================*/
    /* Switch per numero documento        */
    /*------------------------------------*/
    if (!num_bol || num_bol == 0)
    {
        /*--------------------------------*/
        /* Se protocollo da impostare:    */
        /* disabilitazioni                */
        /*--------------------------------*/
        $('#acc_spn').prop("disabled",true);
    }
     else
    {
        /*--------------------------------*/
        /* Se responsabile impostato:     */
        /* abilitazioni                   */
        /*--------------------------------*/
        $('#acc_spn').prop("enabled",true);
        $('#acc_spn').focus()             ;
    }

    /*====================================*/
    /* Highlight numero documento         */
    /*------------------------------------*/
    $('#num_bol').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        cel_css("cel_num", "+")           ;
        cel_css("cel_pro", "-")           ;
    });
    
    /*====================================*/
    /* Cambio Operatore                   */
    /*------------------------------------*/
    $('#rsp_doc').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#num_bol').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Cambio Numero documento            */
    /*------------------------------------*/
    $('#num_bol').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Highlight ubicazioni               */
    /*------------------------------------*/
    $('#acc_spn').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        var cel = $(this).closest("td").attr('id');
        $(this).closest("tr").find('#' + cel).css({"background-color": "#BBBBBB"});
    });

    /*====================================*/
    /* Bottone reset                      */
    /*------------------------------------*/
    $("#res").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#num_bol').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Modifica box di spunta             */
    /*------------------------------------*/
    $('#acc_spn').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WP"                ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val() ;
        var prt_bol = $('#prt_bol').val() ;
        var dat_att = $('#dat_att').val() ;
        var ora_att = $('#ora_att').val() ;
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(this).is(":checked"))
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
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "prt_bol": prt_bol,
            "dat_att": dat_att,
            "ora_att": ora_att,
            "flg_spn": flg_spn
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebolw0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                $('#msg').html(data)      ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $(mod_frm).submit()       ;
            }
        });
    });          
});

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/

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
