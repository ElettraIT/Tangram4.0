/*========================================*/
/* eleodsB2 - Javascript - by N&A         */
/* -------------------------------------- */
/* Aggiornamento   : 12/10/2026           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
mod_frm = "#ods_002"                      ;

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
    /* - '0' : Accettazione responsabile  */
    /* - '1' : Accettazione protocolli    */
    /* - '2' : Accettazione prodotti      */
    /*------------------------------------*/
    liv_ope = $('#liv_ope').val()         ;

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
     else if (liv_ope == 2)
    {
        /*--------------------------------*/
        /* Livello '2'                    */
        /*--------------------------------*/
        liv_002 ()                       ;
    }
     else if (liv_ope == 3)
    {
        /*--------------------------------*/
        /* Livello '3'                    */
        /*--------------------------------*/
        liv_003 ()                       ;
    }

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/
});

/*========================================*/
/* FUNZIONI                               */
/*========================================*/

/*========================================*/
/* Funzione di gestione livello '0'       */
/*                                        */
/* Input Operatore                        */
/*----------------------------------------*/
function liv_000 ()
{
    /*====================================*/
    /* Test iniziale su indirizzo IP per  */
    /* individuare l'operatore            */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    $.ajax({
        type: "POST",
        url : "./getip.pl",
        contentType: "application/x-www-form-urlencoded",
        /*--------------------------------*/
        /* Se indirizzo determinato       */
        /*--------------------------------*/
        success: function (data)
        {
            /*----------------------------*/
            /* Determinazione operatore   */
            /*----------------------------*/
            var ipx_val = data.trim()     ;
            let_ipx(ipx_val)              ;
        }
    });

    /*====================================*/
    /* Cella di accettazione in rosso     */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    cel_css("cel_ute", "R")               ;

    /*====================================*/
    /* Cambio Operatore                   */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    $('#rsp_doc').change(function()
    {
        /*--------------------------------*/
        /* Se impostato                   */
        /*--------------------------------*/
        if ($(this) != '')
        {
            /*----------------------------*/
            /* A livello successivo       */
            /*----------------------------*/
            $('#liv_ope').val('1')        ;
        }
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

/*----------------------------------------*/
/* Fine funzione livello '0'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '1'       */
/*                                        */
/* Input Protocolli                       */
/*----------------------------------------*/
function liv_001 ()
{
    abilita_ok_fase_1();
    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione giacenze per ubicazione */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal   : true,
        title   : 'Scheda prodotto',
        width   : 450,
        height  : 450,
    });

    /*====================================*/
    /* Bottone reset                      */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#res").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#prt_ods').val('')             ;
        $('#num_dst').val('')             ;
        $('#tip_ric').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Focus su protocollo                */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#prt_ods').focus()                 ;
    $('#prt_ods').select()                ;
    cel_css("cel_num", "+")               ;

    /*====================================*/
    /* Highlight protocollo               */
    /*                                    */
    /* Livello '1'                        */
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
    });

    /*====================================*/
    /* Cambio protocollo                  */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#prt_ods').change(function()
    {
        /*--------------------------------*/
        /* Valore del campo in input      */
        /*--------------------------------*/
        var prt = $('#prt_ods').val()     ;
        /*--------------------------------*/
        /* Estrazione del primo carattere */
        /*--------------------------------*/
        var prc = (prt.charAt(0))         ;
        /*--------------------------------*/
        /* Test sul primo carattere       */
        /*--------------------------------*/
        if (prc == 'd' || prc == 'D')
        {
            /*----------------------------*/
            /* Se Distinta                */
            /*----------------------------*/
            $('#tip_ric').val('D')        ;
        }
         else if (prc == 'v' || prc == 'V')
        {
            /*----------------------------*/
            /* Se protocollo da visualiz- */
            /* zare                       */
            /*----------------------------*/
            $('#tip_ric').val('V')        ;
            $('#num_dst').val('')         ;
        }
        else
        
        {
            /*----------------------------*/
            /* Se protocollo e basta      */
            /*----------------------------*/
            $('#tip_ric').val('')         ;
        }
        /*--------------------------------*/
        /* Pulizia stringa protocollo     */
        /*--------------------------------*/
        var ppp = prt.match(/\d+/)        ;
            prn = Number(ppp).toString()  ;
            $('#prt_ods').val(prn)        ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Bottone conferma protocolli        */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/


    /*====================================*/
    /* Eventuale espansione riga lista    */
    /* documenti per elenco responsabili  */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#lst td').click(function()
    {
        /*================================*/
        /* Numero colonna                 */
        /*--------------------------------*/
        var num_cln = $(this).parent().children().index($(this));

        /*================================*/
        /* Se colonna del responsabile    */
        /*--------------------------------*/
        if (num_cln == 5)
        {
            /*----------------------------*/
            /* Identificatore documento   */
            /*----------------------------*/
            var prt_lst = $(this).closest("tr").find('#prt_lst').val();
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: "./eleodsr0",
                contentType: "application/x-www-form-urlencoded",
                data: "prt_lst=" + prt_lst,
                success: function (data)
                {
                      $("#dialog").html(data);
                      $("#dialog").dialog("option","title","Responsabili documento").dialog('open');
                }
            });
        }
    });

/*----------------------------------------*/
/* Fine funzione livello '1'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '2'       */
/*                                        */
/* Input Prodotti                         */
/*----------------------------------------*/
function liv_002 () {
    var form = document.getElementById('ods_002');
    $('#but_fip').on('click', function () {
        $('#liv_ope').val('1'); if (form) { form.submit(); }
    });
    $('#but_ref').on('click', function () {
        if (form) { form.submit(); }
    });
    $('#alf_pro').on('change', function () {
        if (form) { form.submit(); }
    });
    $('.osr-nbx').prop('disabled', false).each(function () {
        $(this).data('saved-ok', this.checked);
    }).on('change', function (event) {
        event.stopImmediatePropagation();
        var box = $(this), previous = box.data('saved-ok');
        var wanted = this.checked;
        box.prop('disabled', true);
        $('#nbx_status').text('Salvataggio OK in corso...');
        $.ajax({type: 'POST', url: './eleodsx1', dataType: 'json',
            timeout: 15000,
            data: {mod_ope: 'R', tip_ope: 'NBX',
                rsp_doc: String($('[name=rsp_doc]').val() || '').trim(),
                prt_ods: String(box.attr('data-num-prt') || '').trim(),
                prg_ods: String(box.attr('data-num-prg') || '').trim(),
                flg_nbx: wanted ? 'S' : 'N'}
        }).done(function (result) {
            if (!result || result.ok !== true) {
                box.prop('checked', previous);
                $('#nbx_status').text('OK non salvato: ' +
                    ((result && result.error) || 'risposta non valida'));
                return;
            }
            box.data('saved-ok', wanted);
            $('#nbx_status').text(result.warning ?
                'Avviso: ' + result.warning : '');
        }).fail(function () {
            box.prop('checked', previous);
            $('#nbx_status').text(
                'Salvataggio non confermato. Ricaricare per verificare.');
        }).always(function () {
            box.prop('disabled', false);
            aggiorna_colore_nbx();
            focus_codice_prodotto();
        });
    });
    aggiorna_colore_nbx();
    abilita_codici_e_ubicazioni();
    abilita_commenti_sn();
    focus_codice_prodotto();
}
function focus_codice_prodotto() {
    var campo = document.getElementById('alf_pro');
    if (campo && !campo.disabled) {
        campo.focus(); campo.select();
    }
}

function liv_003 ()
{
    /*====================================*/
    /* Definizione box per messaggi       */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $("#dlg_ms3").dialog({
        modal    : true,
        bgiframe : true,
        height   : "auto",
        width    : 400,
        autoOpen : false,
        closeOnEscape: false,
        resizable: false,
        title    : '*** ATTENZIONE ***'
    });

    /*====================================*/
    /* Focus su ubicazione e quantita'    */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('#cod_ubi').focus()                 ;
    $('#cod_ubi').select()                ;

    var ubi = $('#ubi').val()             ;
    if (ubi != '')
    {
            $('.numbersOnly').focus()     ;
            $('.numbersOnly').select()    ;

    }

    /*====================================*/
    /* Bottone fine modifica              */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('#but_fip').click(function()
    {
        /*--------------------------------*/
        /* A livello precedente           */
        /*--------------------------------*/
        $('#liv_ope').val('2')            ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Bottone refresh                    */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('#but_ref').click(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Highlight quantita'                */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('.numbersOnly').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
    });

    /*====================================*/
    /* Rilevazione tasto funzione Tangram */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('.numbersOnly').keyup(function(e)
    {
        /*--------------------------------*/
        /* Rilevazione tasto              */
        /*--------------------------------*/
        var tgk = (e.keyCode ? 
	            e.keyCode : 
	            e.which)               ;
        if(tgk == 13){$(mod_frm).submit()};
    });           


    /*====================================*/
    /* Modifica quantita'                 */
    /*                                    */
    /* (livello '3')                      */
    /*------------------------------------*/
    $('.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Codice ubicazione              */
        /*--------------------------------*/
        var cod_ubi = $(this).closest("tr").find('#ubi').val();
        /*--------------------------------*/
        /* Test preliminare se ubicazione */
        /* selezionata                    */
        /*--------------------------------*/
        if (cod_ubi == '')
        {
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            alert ("Selezionare una ubicazione");
            /*----------------------------*/
            /* Normalizzazione quantita'  */
            /*----------------------------*/
            $(this).closest("tr").find('#qtv').val('');
//            $('#qtv').val('')             ;
            $('#cod_ubi').focus()         ;
            $('#cod_ubi').select()        ;
            /*----------------------------*/
            /* Uscita                     */
            /*----------------------------*/
            return                        ;
        }
    
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WU"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_ods = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_ods             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var num_dst = $('#num_dst').val();
        var cod_ubi = $(ide_row).find('#ubi').val();
        var qta_ord = $(ide_row).find('#qta').val();
        var qta_ubi = $(ide_row).find('#qtv').val();
        var num_pro = $(ide_row).find('#pro').val();
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
        if (qta_ubi == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Messaggio di quantita' su-     */
        /* periore a quella in ordine     */
        /*--------------------------------*/
        var dif = qta_ord - qta_ubi       ;
        if (dif < 0)
        {
            /*----------------------------*/
            /* Normalizzazione            */
            /*----------------------------*/
            $(ide_row).find('#qtv').val(0);
            qta_ubi = 0                   ;
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            alert("Quantita' non accettabile!");
            /*----------------------------*/
            /* Uscita                     */
            /*----------------------------*/
            return                        ;
        }

        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm = {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "num_dst": num_dst,
            "cod_ubi": cod_ubi,
            "num_pro": num_pro,
            "qta_ubi": qta_ubi,
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
            url: "./eleodsx0",
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
    });          

    /*====================================*/
    /* Accettazione quantita' verificata: */
    /* scorciatoia per copiare quantita'  */
    /* ordinata                           */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WU"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_ods = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_ods             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var num_dst = $('#num_dst').val();
        var cod_ubi = $(ide_row).find('#ubi').val();
        var num_pro = $(ide_row).find('#pro').val();
        
        
        var num_cln = $(this).parent().children().index($(this));
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked"))
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
        /* Test che sia la colonna della  */
        /* quantita' ordinata e non ci    */
        /* sia gia' la spunta             */
        /*--------------------------------*/
        if (num_cln == 9)
        {
        
            /*----------------------------*/
            /* Test se ubicazione         */
            /*----------------------------*/
            if (cod_ubi =='')
            {
                alert ("Selezionare una ubicazione");
                return                    ;
            }
        
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            var qta_sub = $(this).closest("tr").find('#qta').val();
            $(this).closest("tr").find('#qtv').val(qta_sub);
            
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "num_dst": num_dst,
            "cod_ubi": cod_ubi,
            "num_pro": num_pro,
            "qta_ubi": qta_sub,
            "flg_spn": flg_spn
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm);

        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./eleodsx0",
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
            
        }

    });

    /*====================================*/
    /* Controllo inserimento ubicazione   */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('#cod_ubi').keydown(function(e)
    {
        /*--------------------------------*/
        /* Controllo se tasto enter       */
        /*--------------------------------*/
        if (e.keyCode === 13)
        {
            /*----------------------------*/
            /* Controllo tasto            */
            /*----------------------------*/
            e.preventDefault()            ;
            /*----------------------------*/
            /* Valori da riga tabella     */
            /*----------------------------*/
            var cod_ubi = $('#cod_ubi').val();
                cod_ubi = cod_ubi.trim()  ;
                cod_ubi = pul_str (cod_ubi);
            $('#cod_ubi').val(cod_ubi)    ;
            var num_pro = $('#num_pro').val();
            /*----------------------------*/
            /* Oggetto con i Parametri    */
            /*----------------------------*/
            const prm =
            {
                "cod_ubi": cod_ubi,
                "num_pro": num_pro
            }
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            const dat = $.param(prm)      ;

            /*----------------------------*/
            /* Lettura                    */
            /*----------------------------*/
            $.ajax({ type: 'POST',
                      url: "./eleodsu0",
              contentType: "application/x-www-form-urlencoded",
                     data: dat,
                  success: function(data)
                {
                /*------------------------*/
                /* Valore in output       */
                /*------------------------*/
                let res = data.substring(0, 2);

                /*========================*/
                /* Se 'OK'                */
                /*------------------------*/
                if (res == 'OK')
                {
                    /*--------------------*/
                    /* Esecuzione modulo  */
                    /*--------------------*/
                    $(mod_frm).submit()   ;
                }
                /*========================*/
                /* Se non in giacenza     */
                /*------------------------*/
                else if (res == 'KU')
                {
                    /*--------------------*/
                    /* Messaggio          */
                    /*--------------------*/
                    alert('Prodotto non in ubicazione!');
                    /*--------------------*/
                    /* Normalizzazione    */
                    /* Ubicazione         */
                    /*--------------------*/
                    $('#cod_ubi').val('');
                }
                /*========================*/
                /* Se inesistente         */
                /*------------------------*/
                 else
                {
                    /*--------------------*/
                    /* Messaggio          */
                    /*--------------------*/
                    alert('Ubicazione inesistente');
                    /*--------------------*/
                    /* Normalizzazione    */
                    /* Ubicazione         */
                    /*--------------------*/
                    $('#cod_ubi').val('') ;                
                }
            } 
            /*----------------------------*/
            /* Lettura                    */
            /*                            */
            /* Fine                       */
            /*----------------------------*/
            });
        /*--------------------------------*/
        /* Controllo se tasto enter       */
        /*                                */
        /* Fine                           */
        /*--------------------------------*/
        }
    /*------------------------------------*/
    /* Controllo inserimento ubicazione   */
    /*                                    */
    /* Livello '3'                        */
    /*                                    */
    /* Fine                               */
    /*====================================*/
    });

/*----------------------------------------*/
/* Fine funzione livello '3'              */
/*========================================*/
}

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
/* Funzione di richiesta conferma spunta  */
/*----------------------------------------*/
function cnf_dlg(msg)
{
    /*------------------------------------*/
    /* Messaggio per il box               */
    /*------------------------------------*/
    var dialog = $("<div>"+msg+"</div>")  ;
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
        width    : 600,
        modal    : true,
        buttons  :
        {
            'si': function()
            {
                def.resolve();
                $(dialog).dialog( "close" );
            },
            'NO': function()
            {
                def.reject();
                $(dialog).dialog( "close" );
            }
        },
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
/* Lettura nodo IP per determinare il     */
/* codice operatore                       */
/*----------------------------------------*/
function let_ipx(val_ipx)
{
    /*------------------------------------*/
    /* Normalizzazione nodo IP            */
    /*------------------------------------*/
    var ipx_arr = val_ipx.split(".")      ;
    var ipw = ipx_arr[3]                  ;
    var ipw = pad(ipw, 3)                 ;
    /*------------------------------------*/
    /* Oggetto con i Parametri            */
    /*------------------------------------*/
    const prm =
    {
        "val_ipx": ipw
    }
    /*------------------------------------*/
    /* Assemblaggio parametri             */
    /*------------------------------------*/
    const dat = $.param(prm)              ;
    /*------------------------------------*/
    /* Lettura                            */
    /*------------------------------------*/
    $.ajax({ type: 'POST',
              url: "./elebfoi0",
      contentType: "application/x-www-form-urlencoded",
             data: dat,
          success: function(data)
          {
              /*--------------------------*/
              /* Esito lettura            */
              /*--------------------------*/
              let res = data.substring(0, 3);
                  res = res.trim()        ;
                  res = parseInt(res)     ;
                  $('#rsp_doc').val(res)  ;

              /*--------------------------*/
              /* Test su esito lettura    */
              /*--------------------------*/
              if (res == 0 || !res)
              {
                  /*----------------------*/
                  /* Se non trovato: nor- */
                  /* malizzazione codice  */
                  /* operatore            */
                  /*----------------------*/
                  $('#rsp_doc').val(0)    ;
                  /*----------------------*/
                  /* Uscita               */
                  /*----------------------*/
                    return                ;
              }
               else
              {
                  /*----------------------*/
                  /* Operatore determina- */
                  /* to                   */
                  /*----------------------*/
                  $('#rsp_doc').val(res)  ;
                  /*----------------------*/
                  /* A livello successivo */
                  /*----------------------*/
                  $('#liv_ope').val('1')  ;
                  $(mod_frm).submit()     ;
              }
          }
    });
/*----------------------------------------*/
/* Lettura nodo IP - fine                 */
/*========================================*/
}

/*========================================*/
/* Formattazione numeri con zeri in testa */
/*----------------------------------------*/
function pad (str, max)
{
    str = str.toString()                  ;
    return str.length < max ? pad("0" + str, max) : str;
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

function abilita_ok_fase_1() {
    var form = document.getElementById('ods_002');
    var button = document.getElementById('but_fip');
    if (!form || !button) { return; }
    button.disabled = false;
    button.onclick = function (event) {
        if (event) { event.preventDefault(); }
        var distinta = form.elements.namedItem('num_dst');
        var livello = form.elements.namedItem('liv_ope');
        var numero = distinta ? String(distinta.value).trim() : '';
        if (!livello || !/^\d+$/.test(numero) || Number(numero) === 0) {
            window.alert('Richiamare un ordine aperto e tutto spuntato.');
            return false;
        }
        distinta.value = numero; livello.value = '2';
        form.submit(); return false;
    };
}

/* Modal SN separato dalla spunta NBX e senza click sulle celle vuote. */
function abilita_commenti_sn() {
    $('#dlg_com').dialog({autoOpen: false, modal: true,
        width: 600, height: 450, title: 'S/N prodotto',
        close: function () { focus_codice_prodotto(); }
    });
    $('#rig').on('click', 'td.descr-con-commenti', function (event) {
        event.stopPropagation();
        var cella = $(this);
        $.ajax({type: 'POST', url: './eleodsS1',
            contentType: 'application/x-www-form-urlencoded',
            data: {
                num_prt: String(cella.attr('data-num-prt') || '').trim(),
                num_prg: String(cella.attr('data-num-prg') || '').trim()
            },
            success: function (data) {
                if (data.indexOf('<!-- eleods-sn:0 -->') !== -1) {
                    cella.removeClass('descr-con-commenti')
                        .css({'background-color': '', 'color': '',
                              'cursor': 'default'}).removeAttr('title');
                    focus_codice_prodotto();
                    return;
                }
                $('#dlg_com').html(data).dialog('open');
            },
            error: function () {
                $('#nbx_status').text('Impossibile leggere i commenti SN.');
                focus_codice_prodotto();
            }
        });
    });
}

/* Usa lo stesso evento change del campo in testata. */
function abilita_codici_e_ubicazioni() {
    $('#rig').on('click', 'td.codice-prodotto', function (event) {
        event.stopPropagation();
        var codice = $(this).text().trim();
        if (codice) {
            $('#alf_pro').val(codice).trigger('change');
        }
    });
    $('#dialog').dialog({autoOpen: false, modal: true,
        width: 950, height: 450,
        close: function () { focus_codice_prodotto(); }
    });
    $('#rig').on('click', 'td.ubicazioni-prodotto', function (event) {
        event.stopPropagation();
        var codice = $(this).closest('tr')
            .find('td.codice-prodotto').text().trim();
        if (!codice) { return; }
        $.ajax({type: 'POST', url: './eleodsg0',
            contentType: 'application/x-www-form-urlencoded',
            data: {alf_pro: codice},
            success: function (data) {
                $('#dialog').html(data)
                    .dialog('option', 'title', 'Prodotto: ' + codice)
                    .dialog('open');
            },
            error: function () {
                $('#nbx_status').text('Impossibile leggere le ubicazioni.');
                focus_codice_prodotto();
            }
        });
    });
}

/* Verde solo quando tutti gli NBX dell'elenco sono confermati. */
function aggiorna_colore_nbx() {
    if (!document.getElementById('stile-nbx-completi')) {
        var stile = document.createElement('style');
        stile.id = 'stile-nbx-completi';
        stile.textContent = '#rig tr.nbx-completi > td' +
            ':not(.descr-con-commenti) {' +
            'background-color:#e3f3df !important;}';
        document.head.appendChild(stile);
    }
    var boxes = $('#rig .osr-nbx');
    var completi = boxes.length > 0;
    boxes.each(function () {
        if ($(this).data('saved-ok') !== true) {
            completi = false;
        }
    });
    boxes.closest('tr').toggleClass('nbx-completi', completi);
}
