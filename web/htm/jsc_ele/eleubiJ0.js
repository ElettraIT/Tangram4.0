/*========================================*/
/* eleubij0 - Javascript - N&A            */
/* -------------------------------------- */
/* Funzioni accessorie per cambio ubica-  */
/* zione principale o cancellazione       */
/* giacenza                               */
/* -------------------------------------- */
/* Aggiornamento   : 13/10/2024           */
/* -------------------------------------- */
/* Attualmente utilizzato da elebfoB1     */
/*                           elebfoB2     */
/*                           eleodsB1     */
/*                           elepro01     */
/*                           eleubi01     */
/*========================================*/

/*========================================*/
/* Box di conferma cambio ubicazione      */
/* principale o cancellazione giacenza    */
/*----------------------------------------*/
function dlg_ubi(ope_ric,
                 ubi_pri,
                 num_pro,
                 rsp_des) 
{
    /*------------------------------------*/
    /* Normalizzazione parametri box      */
    /*------------------------------------*/
    var msg_wdt = 500                     ;
    var msg_tit = ""                      ;
    var msg_dia = ""                      ;
    /*------------------------------------*/
    /* Deviazione in funzione del tipo di */
    /* operazione richiesta               */
    /*------------------------------------*/
    if (ope_ric == 'UP')
    {
        /*--------------------------------*/
        /* Cambio ubicazione              */
        /*--------------------------------*/
        msg_wdt = 500                     ;
        msg_tit = "ATTENZIONE!"           ;
        msg_dia = "<div>Vuoi impostare <b>"
                + ubi_pri
                + "</b> come ubicazione "
                + "principale?</div>"     ;
    }
     else if (ope_ric == 'DG')
    {
        /*--------------------------------*/
        /* Cancellazione giacenza         */
        /*--------------------------------*/
        msg_wdt = 600                     ;
        msg_tit = "RICHIESTA CANCELLAZIONE!";
        msg_dia = "<div><h4>Vuoi cancellare "
                + "la giacenza a zero dell'"
                + "ubicazione <b> "
                + ubi_pri
                + "</b> ?<h4></div>"      ;
    }
     else
    {
        /*--------------------------------*/
        /* In tutti gli altri casi        */
        /*--------------------------------*/
        msg_wdt = 600                     ;
        msg_tit = "ATTENZIONE!"           ;
        msg_dia = "<div>Operazione non "
                + "ricionosciuta!</div>"  ;
    }

    /*------------------------------------*/
    /* Emissione dialog                   */
    /*------------------------------------*/
    $(msg_dia).dialog(
    {
        /*--------------------------------*/
        /* Parametri box                  */
        /*--------------------------------*/
        width  : msg_wdt,
        height : "auto",
        title  : msg_tit,
        modal  : true,
        buttons:
        {
            /*============================*/
            /* Si, conferma               */
            /*----------------------------*/
            "Si": function(event)
            {
                /*------------------------*/
                /* Normalizzazione        */
                /*------------------------*/
                 event.preventDefault()   ;
                /*------------------------*/
                /* Nuova ubicazione       */
                /* principale assegnata   */
                /*------------------------*/
                new_ubp(ope_ric,
                        num_pro,
                        ubi_pri,
                        rsp_des)          ;
                /*------------------------*/
                /* Chiusura box           */
                /*------------------------*/
                $(this).dialog("close")   ;
                /*------------------------*/
                /* Submit del modulo      */
                /*------------------------*/
                $(mod_frm).submit()       ;
            },
            /*============================*/
            /* No, annullamento           */
            /*----------------------------*/
            "No": function()
            {
                /*------------------------*/
                /* Chiusura box           */
                /*------------------------*/
                $(this).dialog("close")   ;
            }
        },
        
        /*================================*/
        /* Bottone di default (NO)        */
        /*--------------------------------*/
        open: function()
        {
            $(this).siblings('.ui-dialog-buttonpane').find('button:eq(1)').focus(); 
        }
        
    });
/*----------------------------------------*/
/* Dialogo di conferma - fine             */
/*========================================*/
};

/*========================================*/
/* Funzione di aggiornamento in base al   */
/* tipo operazione                        */
/*----------------------------------------*/
function new_ubp (upw_ope,
                  upw_pro,
                  upw_ubi,
                  upw_res)
{
    /*====================================*/
    /* Oggetto con i Parametri            */
    /*------------------------------------*/
    const prm =
    {
        "tip_ope": upw_ope,
        "num_pro": upw_pro,
        "cod_ubi": upw_ubi,
        "rsp_des": upw_res
    }
    /*------------------------------------*/
    /* Assemblaggio parametri             */
    /*------------------------------------*/
    const dat = $.param(prm)              ;

    /*====================================*/
    /* Chiamata modulo di scrittura       */
    /*------------------------------------*/
    $.ajax({
        /*--------------------------------*/
        /* Richiamo modulo                */
        /*--------------------------------*/
        type: "POST",
        url: "./eleodsx0",
        contentType: "application/x-www-form-urlencoded",
        data: dat,
        /*--------------------------------*/
        /* Se andato a buon fine          */
        /*--------------------------------*/
        success: function (data)
        {
            /*----------------------------*/
            /* ___                        */
            /*----------------------------*/
   //         alert("Ubicazione principale modificata!");            
        }
    });
/*----------------------------------------*/
/* Aggiornamento ubicazione - fine        */
/*========================================*/
};    
    
