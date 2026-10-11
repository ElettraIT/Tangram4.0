/*========================================*/
/* utmp0 - Javascript - by WIP            */
/* -------------------------------------- */
/* Aggiornamento   : 23/02/2025           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
url_exp = "./utmp1.pl"                    ;
url_odo = "./utmp2.pl"                    ;

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione su utente               */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal   : true,
        title   : 'Scheda prodotto',
        width   : 700,
        height  : "auto"
    });

    /*====================================*/
    /* Se tabella vuota                   */
    /*------------------------------------*/
    if ($('#utenti tbody tr').length === 0)
    {
        /*--------------------------------*/
        /* Inibizione tabella utenti      */
        /*--------------------------------*/
        $('#utenti').hide()               ;
        /*--------------------------------*/
        /* Inibizione orologio            */
        /*--------------------------------*/
        $('#ora').hide()                  ;
        /*--------------------------------*/
        /* Inibizione bottone grafico     */
        /*--------------------------------*/
        $('#odo_mtr').hide()              ;
        /*--------------------------------*/
        /* Messaggio                      */
        /*--------------------------------*/
        var msg = 'Nessun utente collegato';
        $('#msg').text(msg)               ;
    }

    /*====================================*/
    /* Aggiornamento orologio             */
    /*------------------------------------*/
    setInterval(agg_ora, 20000)           ;
    agg_ora()                             ;

    /*====================================*/
    /* Bottone indicatore utenti attivi   */
    /*------------------------------------*/
    $("#odo_mtr").click(function()
    {
        /*--------------------------------*/
        /* Titolo Dialog                  */
        /*--------------------------------*/
        var dlg_tit = "Numero utenti attivi";
        /*--------------------------------*/
        /* Numero utenti attivi           */
        /*--------------------------------*/
        var ctr_usr = $("#ctr_ele").data("id");
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "ctr_usr": ctr_usr
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;
        /*--------------------------------*/
        /* Riempimento box                */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url : url_odo,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            success: function (data)
            {
                $("#dialog").html(data);
                $("#dialog").dialog("option","title",dlg_tit).dialog('open');
            }
        });
        
        
        
        



    });

    /*====================================*/
    /* Controllo click su elementi riga   */
    /*------------------------------------*/
    $('#utenti td').click(function()
//    $('#utenti td').hover(function()
    {
        /*--------------------------------*/
        /* Indicatore colonna             */
        /*--------------------------------*/
        var cln_des = 1                   ;
        /*--------------------------------*/
        /* Codice selezionato             */
        /*--------------------------------*/
        var cod_usr = $(this).closest("tr").data("id");
        /*--------------------------------*/
        /* Colonna in cui e' avvenuto il  */
        /* click                          */
        /*--------------------------------*/
        var num_cln = $(this).parent().children().index($(this));

        /*================================*/
        /* Test che sia la colonna della  */
        /* descrizione utente             */
        /*--------------------------------*/
        if (num_cln == cln_des)
        {
            /*----------------------------*/
            /* Oggetto con i Parametri    */
            /*----------------------------*/
            const prm =
            {
                "cod_usr": cod_usr
            }
            /*----------------------------*/
            /* Titolo Dialog              */
            /*----------------------------*/
            var dlg_tit = "Utente : " 
                        + cod_usr         ;
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            const dat = $.param(prm)      ;
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url : url_exp,
                contentType: "application/x-www-form-urlencoded",
                data: dat,
                success: function (data)
                {
                    $("#dialog").html(data);
                    $("#dialog").dialog("option","title",dlg_tit).dialog('open');
                }
            });
        }
    /*------------------------------------*/
    /* Controllo click su elementi riga   */
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
/* Aggiornamento ora                      */
/*----------------------------------------*/
function agg_ora()
{
    /*====================================*/
    /* Variabili                          */
    /*------------------------------------*/
    var ora = new Date()                  ;
    var ore = String(ora.getHours()).padStart(2, '0');
    var min = String(ora.getMinutes()).padStart(2, '0');
    var sec = String(ora.getSeconds()).padStart(2, '0');
    /*------------------------------------*/
    /* Aggiorna contenuto elemento 'ora'  */
    /*------------------------------------*/
    $("#ora").text(ore + ":" + min + "." + sec);
/*----------------------------------------*/
/* Aggiornamento - FINE                   */
/*========================================*/
}
