/*========================================*/
/* ele_log - Javascript - by WIP          */
/* -------------------------------------- */
/* Aggiornamento   : 19/07/2024           */
/*========================================*/

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function() {

    /*------------------------------------*/
    /* Inizializzazione Form              */
    /*------------------------------------*/
    $('#log_inn')[0].reset()              ;
    /*------------------------------------*/
    /* Inibizione iniziale bottoni        */
    /*------------------------------------*/
    $('input[type="submit"]').attr('disabled','disabled');
    /*------------------------------------*/
    /* Focus su utente                    */
    /*------------------------------------*/
    $('#cod_ute').focus()                 ;

    /*------------------------------------*/
    /* Valori da campi Form               */
    /*------------------------------------*/
    var ute = $("#cod_ute").val()         ;
    var pwd = $("#pwd_ute").val()         ;

    /*====================================*/
    /* Test su campo utente               */
    /*------------------------------------*/
    $('#cod_ute').change(function()
    {
        /*--------------------------------*/
        /* Valore campo                   */
        /*--------------------------------*/
        var input=$(this);
        var is_name=input.val();
        /*--------------------------------*/
        /* Se vuoto                       */
        /*--------------------------------*/
        if(!is_name)
        {
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            var text   = "ATTENZIONE: <b>Inserire utente!</b>";
            $('#info').html(text);
            /*----------------------------*/
            /* Disabilito 'submit'        */
            /*----------------------------*/
            $('input[type="submit"]').attr('disabled','disabled');
        }
        /*--------------------------------*/
        /* Se scelto                      */
        /*--------------------------------*/
         else
        {
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
//            var text   = "Utente: <b>" + is_name + "</b>";
            $('#info').html(text);
            /*----------------------------*/
            /* Abilito 'submit'           */
            /*----------------------------*/
            $('input[type="file"]').removeAttr('disabled');
        }
        
    });

    /*====================================*/
    /* Test su campo password             */
    /*------------------------------------*/
    $('#pwd_ute').change(function()
    {
        /*--------------------------------*/
        /* Valore campo                   */
        /*--------------------------------*/
        var input=$(this);
        var is_name=input.val();
        /*--------------------------------*/
        /* Se vuoto                       */
        /*--------------------------------*/
        if(!is_name)
        {
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            var text   = "ATTENZIONE: <b>Inserire password!</b>";
            $('#info').html(text);
            /*----------------------------*/
            /* Disabilito 'submit'        */
            /*----------------------------*/
            $('input[type="submit"]').attr('disabled','disabled');
        }
        /*--------------------------------*/
        /* Se scelto                      */
        /*--------------------------------*/
         else
        {
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
//            var text   = "Password inserita";
//            $('#info').html(text);
            /*----------------------------*/
            /* Abilito 'submit'           */
            /*----------------------------*/
            if(ute != '')
            {
                $('input[type="submit"]').removeAttr('disabled');
            }
        }
    });

    /*====================================*/
    /* Se premuto tasto conferma          */
    /*------------------------------------*/
    $("#conferma").click(function() {
        /*--------------------------------*/
        /* Valori da campi Form           */
        /*--------------------------------*/
        var cod_ute = $("#cod_ute").val();
        var pwd_ute = $("#pwd_ute").val();
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "cod_ute": cod_ute,
            "pwd_ute": pwd_ute
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;
        /*--------------------------------*/
        /* Chiamata modulo lettura        */
        /*--------------------------------*/
        $.ajax({
            type : 'GET',
            url  : 'ele_log_b.pl',
            async: false,
            contentType: "application/json; charset=utf-8",
            dataType   : "json",
            data       : dat,
            /*----------------------------*/
            /* Lettura ok                 */
            /*----------------------------*/
            success: function(data)
            {
                /*------------------------*/
                /* Controllo risultato    */
                /*------------------------*/
                if (data.error)
                {
                    /*--------------------*/
                    /* Messaggio          */
                    /*--------------------*/
                    $('#info').html(data.error);
                    /*--------------------*/
                    /* Disabilito submit  */
                    /*--------------------*/
                    $('input[type="submit"]').attr('disabled','disabled');
                }
                 else
                {
                    /*--------------------*/
                    /* Trattamento login  */
                    /*--------------------*/
                    $('#log_inn').submit();
                }
            },
            /*----------------------------*/
            /* Se errore di lettura       */
            /*----------------------------*/
            error: function (request, status, error)
            {
                $("#info").html("ERRORE di lettura!");
            }
        });
    });

    /*====================================*/
    /* Tasto Return                       */
    /*------------------------------------*/
//    $(document).keypress(function( event )
//    {
//        if ( event.which == 13 )
//        {
//            $('#loginForm').submit()      ;
//        }    
//    });

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/
});

/*========================================*/
/* FUNZIONI                               */
/*========================================*/
