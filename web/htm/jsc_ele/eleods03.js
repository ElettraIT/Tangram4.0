/*========================================*/
/* eleods03 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 17/09/2023           */
/*                                        */
/* ___NON PIU' UTILIZZATO ___             */
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Protocollo ordine                  */
    /*------------------------------------*/
    prt_ods = $('#prt_ods').val()         ;
    /*------------------------------------*/
    /* Responsabile                       */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    rsp_des = $('#rsp_doc option:selected').text();

    /*====================================*/
    /* Eventuale switch iniziale campi    */
    /*------------------------------------*/
    if (!rsp_doc || rsp_doc == 0)
    {
        /*--------------------------------*/
        /* Se responsabile da impostare:  */
        /* disabilitazioni                */
        /*--------------------------------*/
        $('#prt_ods').prop("disabled",true);
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
    }
     else
    {
        /*--------------------------------*/
        /* Se responsabile impostato:     */
        /* abilitazioni                   */
        /*--------------------------------*/
        $('#prt_ods').prop("enabled",true);
        $('#acc_spn').prop("enabled",true);
        cel_css("cel_num", "+")           ;
        $('#prt_ods').focus()             ;
    }

    /*====================================*/
    /* Switch per protocollo              */
    /*------------------------------------*/
    if (!prt_ods || prt_ods == 0)
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
    /* Highlight Numero protocollo        */
    /*------------------------------------*/
    $('#prt_ods').click(function()
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
        $('#prt_ods').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_003").submit()            ;
    });          

    /*====================================*/
    /* Cambio Numero documento            */
    /*------------------------------------*/
    $('#prt_ods').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_003").submit()            ;
    });          

    /*====================================*/
    /* Forzatura campi numerici           */
    /* (classe: numbersOnly)              */
    /*------------------------------------*/
    $('.numbersOnly').keyup(function () { 
        this.value = this.value.replace(/[^0-9\.]/g,'');
    });

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
    /* Modifica box di spunta o quantita''*/
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
        var rsp_doc = $('#rsm').val()     ;
        var prt_ods = $('#prt').val()     ;
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
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_doc=" + rsp_doc ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "prt_ods=" + prt_ods ;
        var prm_04 = "flg_spn=" + flg_spn ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04                  ;
                  
                  
                  
// console.log(prm);
                  
                  
                  
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./eleodsw0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                $('#msg').html(data)      ;
                
                
// console.log(data);
                
                
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#ods_003").submit()    ;
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

