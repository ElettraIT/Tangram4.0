/*========================================*/
/* elebfoB2 - Javascript - N&A            */
/* -------------------------------------- */
/* Aggiornamento   : 10/10/2024           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
mod_frm = "#bfo_002"                      ;

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
    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione elenco 'check'          */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal: true,
        title: 'Scheda prodotto',
        width: 950,
        height: "auto",
    });

    /*====================================*/
    /* Numero documento                   */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    num_doc = $('#num_doc').val()         ;

    if (num_doc == '')
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_num", "+")           ;
        $('#num_doc').focus()             ;
    }
    
    /*====================================*/
    /* Responsabile                       */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    rsp_des = $('#rsp_doc option:selected').text();
    /*------------------------------------*/
    /* Flag di stampa                     */
    /*------------------------------------*/
    flg_stp = $('#flg_stp').val()         ;

    /*====================================*/
    /* Highlight Numero documento         */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#num_doc').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_num", "+")           ;
    });
    
    /*====================================*/
    /* Cambio Numero documento            */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#num_doc').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')             ;
        $('#prt_bfo').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Highlight Codice prodotto          */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#alf_pro').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_pro", "+")           ;
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
        $('#num_doc').val('')             ;
        $('#prt_bfo').val('')             ;
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Bottone reset codice prodotto      */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#rep").click(function()
    {
        /*--------------------------------*/
        /* Se prodotto inserito           */
        /*--------------------------------*/
        var alf = $('#alf_pro').val()     ;
        if (alf != '')
        {
            /*----------------------------*/
            /* Normalizzazione livello    */
            /* operativo                  */
            /*----------------------------*/
            $('#liv_ope').val('')         ;
            /*----------------------------*/
            /* Normalizzazione prodotto   */
            /*----------------------------*/
            $('#alf_pro').val('')         ;
            /*----------------------------*/
            /* Caricamento del modulo     */
            /*----------------------------*/
            $(mod_frm).submit()           ;
        }
    });

    /*====================================*/
    /* Inibizione totale se stampante     */
    /* verde                              */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    if (flg_stp != '#')
    {
        $('input[type=checkbox]').attr('disabled','true');
    }

    /*====================================*/
    /* Eventuale switch bottone stampa    */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    if (flg_stp == "#" || flg_stp == undefined)
    {
        cel_css("stp", "R")               ;
        $('#stp').prop("disabled",true)   ;
    }
     else
    {
        cel_css("stp", "G")               ;
        $('#stp').prop("enabled",true)    ;
    }
    
    /*====================================*/
    /* Eventuale inibizione codice        */
    /* prodotto                           */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
//    if (!num_doc || num_doc == "")
//    {
//        $('#alf_pro').prop("disabled",true);
//    }
//     else
//    {
//        $('#alf_pro').prop("enabled",true);
//    }
    
    /*====================================*/
    /* Focus su prodotto                  */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
//    $('#alf_pro').focus()                 ;
//    $('#alf_pro').select()                ;

    /*====================================*/
    /* Cambio prodotto                    */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $("#prt_bfo").val() ;
        var alf_pro = $('#alf_pro').val() ;
        /*--------------------------------*/
        /* Valori numerico protocollo     */
        /*--------------------------------*/
        var prt_num = parseInt(prt_bfo)   ;
        /*--------------------------------*/
        /* Test se presente o meno il     */
        /* protocollo documento           */
        /*--------------------------------*/
        if (prt_num == 0)
        {
            /*============================*/
            /* Se manca il protocollo     */
            /*                            */
            /* Ricerca per prodotto nei   */
            /* documenti 'aperti'         */
            /*----------------------------*/
            ric_pro(alf_pro)              ;
        }
         else
        {
            /*============================*/
            /* Se c'e' il protocollo      */
            /*                            */
            /* Lettura prodotto           */
            /*----------------------------*/
            var snx_pro = let_pro(alf_pro);
            /*----------------------------*/
            /* Se prodotto trovato        */
            /*----------------------------*/
            if (snx_pro == 'OK')
            {
                /*------------------------*/
                /* Livello successivo     */
                /*------------------------*/
                $('#liv_ope').val('2')    ;
            }
            /*----------------------------*/
            /* Caricamento del modulo     */
            /*----------------------------*/
            $(mod_frm).submit()           ;
        }
    });          

    /*====================================*/
    /* Recupero valori della variabile    */
    /* hidden in array                    */
    /*------------------------------------*/
    var ubi_vrt = $('#ubi_vrt').val()     ;
    var ubi_non = ubi_vrt.split(',').map(ubi => ubi.trim());

    /*====================================*/
    /* Gestione click nella riga          */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Indicatori colonne             */
        /*--------------------------------*/
        var cln_pro = 16                  ;
        var cln_col = cln_pro + 1         ;
        var cln_qta = cln_pro + 4         ;
        var cln_ubi = cln_pro + 7         ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Numero colonna                 */
        /*--------------------------------*/
        var num_cln = $(this).parent().children().index($(this));
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var alf_pro = $(ide_row).find('#alf').val();
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
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

        /*================================*/
        /* Se colonna del codice prodotto */
        /*--------------------------------*/
        if (num_cln == cln_pro)
        {
            /*----------------------------*/
            /* A livello successivo solo  */
            /* se NON spuntata            */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                /*------------------------*/
                /* Progressivo riga       */
                /*------------------------*/
                $('#prg_bfo').val(prg_bfo);
                /*------------------------*/
                /* Codice prodotto        */
                /*------------------------*/
                $('#alf_pro').val(alf_pro);
                /*------------------------*/
                /* A livello successivo   */
                /*------------------------*/
                $('#liv_ope').val('2')    ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $(mod_frm).submit()       ;
            }
        }

        /*================================*/
        /* Se colonna della descrizione:  */
        /* espansione su colli            */
        /*--------------------------------*/
        if (num_cln == cln_col)
        {
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: "./elebfoc0",
                contentType: "application/x-www-form-urlencoded",
                data: "prt_bfo="  + prt_bfo +
                      "&prg_bfo=" + prg_bfo,
                success: function (data)
                {
                      $("#dialog").html(data);
                      $("#dialog").dialog("option","title","Colli della riga").dialog('open');
                }
            });
        }

        /*================================*/
        /* Se colonna della quantita'     */
        /* ordinata e non ci sia gia' la  */
        /* spunta                         */
        /*--------------------------------*/
        if (num_cln == cln_qta)
        {
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                var qta_bfo = $(this).closest("tr").find('#qta').val();
                $(this).closest("tr").find('#qtv').val(qta_bfo);
            }
        }

        /*================================*/
        /* Se colonna dell'ubicazione     */
        /*--------------------------------*/
        if (num_cln == cln_ubi)
        {
            /*----------------------------*/
            /* Parametri                  */
            /*----------------------------*/
            var row_rif = $(this).closest("tr");
            var alf_pro = row_rif.find('#alf').val();
            var num_pro = row_rif.find('#pro').val();
            var ubi_prh = row_rif.find('#ubp').val();
            var rsp_des = $('#rsp_des').val();
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: "./eleodsg0",
                contentType: "application/x-www-form-urlencoded",
                data: { alf_pro: alf_pro },
                success: function(data) {
                    $("#dialog").html(data);
                    $("#dialog").dialog({
                        title: `Prodotto: ${alf_pro}`,
                        open: function() {
                        /*----------------*/
                        /* Assegna evento */
                        /* click primo td */
                        /*----------------*/
                        $('#dialog table tr td:first-child').on('click', function() {
                            var ubi_pri = $(this).text().trim();
                            /*------------*/
                            /* Ubicazioni */
                            /* di trans.  */
                            /*------------*/
                            if (ubi_prh === ubi_pri || ubi_non.includes(ubi_pri))
                                {return;}
                            /*------------*/
                            /* Tipo       */
                            /* operazione */
                            /*------------*/
                            var tip_ope = "UP";
                            /*------------*/
                            /* Conferma   */
                            /*            */
                            /* Funzione   */
                            /* 'eleubiJ0' */
                            /*------------*/
                            dlg_ubi(tip_ope,
                                    ubi_pri,
                                    num_pro,
                                    rsp_des);
                            });
                        }
                    }).dialog('open');
                }
            });
        }
    /*------------------------------------*/
    /* Gestione click nella riga - fine   */
    /*                                    */
    /* Livello '1'                        */
    /*====================================*/
    });

    /*====================================*/
    /* Highlight prodotto                 */
    /*                                    */
    /* Livello '1'                        */
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
    /* Rilevazione tasto funzione Tangram */
    /*                                    */
    /* Livello '1'                        */
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
    /* Modifica box di spunta             */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('input[type="checkbox"]').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WC"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var qta_bfo = $(ide_row).find('#qta').val();
        var qtv_bfo = $(ide_row).find('#qtv').val();
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
            $(ide_row).find('#qta').prop("disabled",true);
            $(ide_row).find('#ann').prop("disabled",true);
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
            $(ide_row).find('#qta').prop("disabled",false);
            $(ide_row).find('#ann').prop("disabled",false);
        }

        /*--------------------------------*/
        /* Messaggio di quantita' a zero  */
        /*--------------------------------*/
        if (qtv_bfo == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "prt_bfo": prt_bfo,
            "prg_bfo": prg_bfo,
            "pri_bfo": pri_bfo,
            "qta_bfo": qta_bfo,
            "qtv_bfo": qtv_bfo,
            "flg_spn": flg_spn
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;

        /*--------------------------------*/
        /* Messaggio per la richiesta di  */
        /* conferma                       */
        /*--------------------------------*/
        var cnf_msg = ''                  ;
        if (flg_spn == 'N')
        {
            cnf_msg = 'Vuoi togliere la spunta al prodotto?'
        }
         else
        {
            cnf_msg = 'Vuoi spuntare manualmente il prodotto?'
        }

        /*================================*/
        /* Richiesta di conferma spunta   */
        /*--------------------------------*/
        cnf_dlg(cnf_msg).done(function()
        {
        /*================================*/
        /* Scelta 'SI'                    */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfox0",
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
            /* Annullamento spunta        */
            /*----------------------------*/
            $(this).prop('checked',false);
            /*----------------------------*/
            /* Ricarica modulo            */
            /*----------------------------*/
            $(mod_frm).submit()           ;
        });
    });          

    /*====================================*/
    /* Modifica quantita'                 */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WC"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var qta_bfo = $(ide_row).find('#qta').val();
        var qtv_bfo = $(ide_row).find('#qtv').val();
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
            $(ide_row).find('#qta').prop("disabled",true);
            $(ide_row).find('#ann').prop("disabled",true);
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
            $(ide_row).find('#qta').prop("disabled",false);
            $(ide_row).find('#ann').prop("disabled",false);
        }

        /*--------------------------------*/
        /* Messaggio di quantita' a zero  */
        /*--------------------------------*/
        if (qtv_bfo == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Messaggio di quantita' su-     */
        /* periore a quella in ordine     */
        /*--------------------------------*/
        if (qtv_bfo > qta_bfo)
        {
            /*----------------------------*/
            /* Normalizzazione            */
            /*----------------------------*/
            $(ide_row).find('#qtv').val(0);
            qtv_bfo = 0                   ;
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
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "prt_bfo": prt_bfo,
            "prg_bfo": prg_bfo,
            "pri_bfo": pri_bfo,
            "qta_bfo": qta_bfo,
            "qtv_bfo": qtv_bfo,
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
            url: "./elebfox0",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                $('#msg').html(data)      ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $(mod_frm).submit()       ;
            }
        });
    });          

    /*====================================*/
    /* Bottone Non conformita'            */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.ncf').click(function()
    {
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_rw2 = "#rw2_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Apertura / chiusura riga       */
        /*--------------------------------*/
        $(ide_rw2).toggle()               ;
        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({"background-color": "#FFFFFF"});
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        $(this).closest("tr").find('#cel_ncf').css({"background-color": "#BBBBBB"});
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var ide_rw4 = "#rw4_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Apertura / chiusura riga       */
        /*--------------------------------*/
        $(ide_rw4).toggle()               ;
        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({"background-color": "#FFFFFF"});
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        $(this).closest("tr").find('#cel_ncf').css({"background-color": "#BBBBBB"});
    });

    /*====================================*/
    /* Accettazione testo Non conformita' */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.inp_ncf').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WN"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#rw2_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var pri_bfo = $(ide_row).find('#pri').val();
        var ann_ncf = $(ide_row).find('#nct').val();
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_des=" + rsp_des ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "prt_bfo=" + prt_bfo ;
        var prm_04 = "prg_bfo=" + prg_bfo ;
        var prm_05 = "pri_bfo=" + pri_bfo ;
        var prm_06 = "ann_ncf=" + ann_ncf ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04 + "&" +
                  prm_05 + "&" +
                  prm_06                  ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfox0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                $('#msg').html(data)      ;
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $(mod_frm).submit()       ;
            }
        });
    });          

    /*====================================*/
    /* Accettazione Barcode               */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.acc_klb').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WB"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#rw3_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var pri_bfo = $(ide_row).find('#pri').val();
        var klb_pro = $(ide_row).find('#klb').val();
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_des": rsp_des,
            "tip_ope": tip_ope,
            "prt_bfo": prt_bfo,
            "prg_bfo": prg_bfo,
            "pri_bfo": pri_bfo,
            "klb_pro": klb_pro
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
            url: "./elebfox0",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                $('#msg').html(data)      ;
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $(mod_frm).submit()       ;
            }
        });
    });          

    /*====================================*/
    /* Bottone stampa                     */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#stp").click(function()
    {
        /*--------------------------------*/
        /* Protocollo                     */
        /*--------------------------------*/
        var prt_bfo = $('#prt').val()     ;
        /*--------------------------------*/
        /* Disabilitazione temporanea     */
        /*--------------------------------*/
//        $('#stp').prop("disabled",true)   ;
        /*--------------------------------*/
        /* Messaggio di stampa in corso   */
        /*--------------------------------*/
        var msg_stp = "<h1>Esecuzione " +
                      "stampa documento " +
                      "in corso ...</h1"  ;
        $('#msg').html(msg_stp)           ;
        /*--------------------------------*/
        /* Attesa di pochi secondi        */
        /*--------------------------------*/
        $('#stp').slideUp(1200).fadeIn(1200);
        /*--------------------------------*/
        /* Stampa                         */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo di stampa  */
            /*----------------------------*/
            type: "POST",
            url: "./elebfo1s",
            contentType: "application/x-www-form-urlencoded",
            data: "prt_bfo=" +
                   prt_bfo,
            /*----------------------------*/
            /* Se andata a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                $('#msg').html(data)      ;
            }
        });
    });

    /*====================================*/
    /* Colorazione di rosso ubicazione    */
    /* principale ed eventuale evidenza   */
    /* di codice e descrizione in presen- */
    /* za dei codici ubicazione 'B'       */
    /*                                    */
    /* (Giorgio 12/06/24)                 */
    /*                                    */
    /* Scansione di tutte le righe        */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#rig tr').each(function()
    {
        /*================================*/
        /* Riferimento alla riga          */
        /*--------------------------------*/
        var $row = $(this)                ;

        /*================================*/
        /* Estrazione literal ubicazione  */
        /*--------------------------------*/
        var lit_ubi = $row.find('#lit_ubi').text();
        /*================================*/
        /* Creazione array dal literal    */
        /*--------------------------------*/
        var ubi_arr = lit_ubi.split(',')  ;
        /*--------------------------------*/
        /* Controllo ubicazioni che       */
        /* iniziano con 'B'               */
        /*--------------------------------*/
        var ubi_bel = ubi_arr.some(function(e)
        {
            return e.trim().toLowerCase().startsWith('b');
        });
        /*--------------------------------*/
        /* Se trovato iniziale 'B'        */
        /*--------------------------------*/
        if (ubi_bel)
        {
            /*----------------------------*/
            /* Cambia il background delle */
            /* celle codice e descrizione */
            /*----------------------------*/
            $row.find('td').eq(1).css('background-color', 'lightblue');
            $row.find('td').eq(2).css('background-color', 'lightblue');
        }

        /*================================*/
        /* Estrazione ubicazione princi-  */
        /* pale                           */
        /*--------------------------------*/
        var pri_ubi = $row.find('#ubp').val();
        /*--------------------------------*/
        /* Pulizia literal ubicazione     */
        /*--------------------------------*/
        lit_ubi = lit_ubi.trim()          ;
        /*--------------------------------*/
        /* Pulizia literal ubicazione     */
        /*--------------------------------*/
        if (lit_ubi !== '')
        {
            /*----------------------------*/
            /* Individuazione eventuale   */
            /* ubicazione principale      */
            /*----------------------------*/
            if (lit_ubi.includes(pri_ubi)) 
            {
                /*------------------------*/
                /* Sostituzione ubicazione*/
                /* principale             */
                /*------------------------*/
                var inx_wrd = lit_ubi.replace(pri_ubi, `<span style="color: red;">${pri_ubi}</span>`);
                lit_ubi = inx_wrd         ;
                /*------------------------*/
                /* Scrittura nuova        */
                /* stringa con ubicazioni */
                /* in grassetto           */
                /*------------------------*/
                $row.find('#lit_ubi').html(lit_ubi);
                $row.find('#lit_ubi').css('font-weight', 'bold');
            }    
        } 
        else 
        {
            /*----------------------------*/
            /* Stringa vuota              */
            /*----------------------------*/
            $row.find('#lit_ubi').html('');
        }
        /*================================*/
        /* Ciclo per lettura righe ed     */
        /* eventuale colorazione grigia   */
        /*--------------------------------*/
        ubi_arr.forEach(function(e) {
            /*----------------------------*/
            /* Rimuovo caratteri laterali */
            /*----------------------------*/
            var ubi_trm = e.trim();
            /*----------------------------*/
            /* Se valore corrisponde a non*/
            /* modificabili, cambia colore*/
            /*----------------------------*/
            if (ubi_non.includes(ubi_trm.toUpperCase())) 
            {
                /*------------------------*/
                /* Sostituisco valore     */
                /* colorato in grigio     */
                /*------------------------*/
                var inx_wrd = lit_ubi.replace(ubi_trm, `<span style="color: grey;">${ubi_trm}</span>`);
                lit_ubi = inx_wrd         ;
                /*------------------------*/
                /* Scrittura nuova        */
                /* stringa con ubicazioni */
                /* in grassetto           */
                /*------------------------*/
                $row.find('#lit_ubi').html(lit_ubi);
                $row.find('#lit_ubi').css('font-weight', 'bold');
            }
        });

    /*------------------------------------*/
    /* Colorazione di rosso ubicazione    */
    /*                                    */
    /* Livello '1'                        */
    /*                                    */
    /* Fine                               */
    /*====================================*/
    });

/*----------------------------------------*/
/* Fine funzione livello '1'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '2'       */
/*                                        */
/* Input quantita' per ubicazione         */
/*----------------------------------------*/
function liv_002 ()
{
    /*====================================*/
    /* Focus su ubicazione e quantita'    */
    /*                                    */
    /* Livello '2'                        */
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
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#but_fip').click(function()
    {
        /*--------------------------------*/
        /* A livello precedente           */
        /*--------------------------------*/
        $('#liv_ope').val('1')            ;
        /*--------------------------------*/
        /* Si pone il contrassegno collo  */
        /* nel numero documento           */
        /*--------------------------------*/
        var num_col = $('#num_col').val() ;
        $('#num_doc').val(num_col)        ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Bottone refresh                    */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#but_ref').click(function()
    {
        /*--------------------------------*/
        /* Si pone il contrassegno collo  */
        /* nel numero documento           */
        /*--------------------------------*/
        var num_col = $('#num_col').val() ;
        $('#num_doc').val(num_col)        ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Highlight quantita'                */
    /*                                    */
    /* Livello '2'                        */
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
    /* Livello '2'                        */
    /*------------------------------------*/
    $('.numbersOnly').keyup(function(e)
    {
        /*--------------------------------*/
        /* Rilevazione tasto              */
        /*--------------------------------*/
        var tgk = (e.keyCode ? 
	            e.keyCode : 
	            e.which)               ;
        /*--------------------------------*/
        /* Se 'invio' o 'return'          */
        /*--------------------------------*/
        if(tgk == 13)
          {$(mod_frm).submit()}           ;
    });           

    /*====================================*/
    /* Modifica quantita'                 */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Codice ubicazione              */
        /*--------------------------------*/
        var cod_ubi = $(this).closest("tr").find('#ubi').val();
        /*--------------------------------*/
        /* Si pone il contrassegno collo  */
        /* nel numero documento           */
        /*--------------------------------*/
        var num_col = $('#num_col').val() ;
        $('#num_doc').val(num_col)        ;
        /*--------------------------------*/
        /* Test preliminare se ubicazione */
        /* selezionata                    */
        /*--------------------------------*/
        if (cod_ubi =='')
        {
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            alert ("Selezionare una ubicazione");
            /*----------------------------*/
            /* Normalizzazione quantita'  */
            /*----------------------------*/
            $('#qtv').val('')             ;
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
        /* Numero documento               */
        /*--------------------------------*/
        num_doc = $('#num_doc').val()     ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
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
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "prt_bfo": prt_bfo,
            "prg_bfo": prg_bfo,
            "cod_ubi": cod_ubi,
            "num_pro": num_pro,
            "qta_ubi": qta_ubi
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
            url: "./elebfox0",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* A livello precedente   */
                /*------------------------*/
 //               $('#liv_ope').val('2')    ;
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
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WU"                ;
        /*--------------------------------*/
        /* Numero documento               */
        /*--------------------------------*/
        num_doc = $('#num_doc').val()     ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg_bfo').val();
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
        if (num_cln == 11)
        {
            /*----------------------------*/
            /* Test se ubicazione         */
            /*----------------------------*/
            if (cod_ubi =='')
            {
                alert ("Selezionare una ubicazione");
                $(mod_frm).submit()       ;
            }
        
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            var qta_sub = $(this).closest("tr").find('#qta').val();
            $(this).closest("tr").find('#qtv').val(qta_sub);

            /*----------------------------*/
            /* Oggetto con i Parametri    */
            /*----------------------------*/
            const prm =
            {
                "rsp_doc": rsp_doc,
                "tip_ope": tip_ope,
                "prt_bfo": prt_bfo,
                "prg_bfo": prg_bfo,
                "cod_ubi": cod_ubi,
                "num_pro": num_pro,
                "qta_ubi": qta_sub
            }
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            const dat = $.param(prm)      ;

            /*----------------------------*/
            /* Aggiornamento Tangram      */
            /*----------------------------*/
            $.ajax({
                /*------------------------*/
                /* Richiamo modulo        */
                /*------------------------*/
                type: "POST",
                url: "./elebfox0",
                contentType: "application/x-www-form-urlencoded",
                data: dat,
                /*------------------------*/
                /* Se andato a buon fine  */
                /*------------------------*/
                success: function (data)
                {
                    /*--------------------*/
                    /* Ricarica modulo    */
                    /*--------------------*/
                    $(mod_frm).submit()    ;
                }
            });
        }
    });

    /*====================================*/
    /* Controllo inserimento ubicazione   */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#cod_ubi').keydown(function(e)
    {
        /*--------------------------------*/
        /* Controllo se tasto enter       */
        /*--------------------------------*/
        if (e.keyCode === 13)
        {
            /*----------------------------*/
            /* Controllo se tasto enter   */
            /*----------------------------*/
            e.preventDefault()            ;
            /*----------------------------*/
            /* Valori da riga tabella     */
            /*----------------------------*/
            var cod_ubi = $('#cod_ubi').val();
                cod_ubi = cod_ubi.trim()  ;
                cod_ubi = pul_str (cod_ubi);

            var num_pro = $('#num_pro').val();
            var prg_bfo = $('#prg_bfo').val();
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
                    /* Progressivo riga   */
                    /*--------------------*/
                    $('#prg_bfo').val(prg_bfo);
                    /*--------------------*/
                    /* Contrassegno collo */
                    /*--------------------*/
                    var num_col = $('#num_col').val();
                    $('#num_doc').val(num_col);
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
           //         $('#cod_ubi').val('');
                    /*--------------------*/
                    /* Progressivo riga   */
                    /*--------------------*/
                    $('#prg_bfo').val(prg_bfo);
                    /*--------------------*/
                    /* Esecuzione modulo  */
                    /*--------------------*/
                    $(mod_frm).submit()   ;
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
    /* Livello '2'                        */
    /*                                    */
    /* Fine                               */
    /*====================================*/
    });

/*----------------------------------------*/
/* Fine funzione livello '2'              */
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
        title    : "Attenzione !",
        modal    : true,
        buttons:
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
/* Lettura codice prodotto                */
/*----------------------------------------*/
function let_pro(alf)
{
    /*------------------------------------*/
    /* Oggetto con i Parametri            */
    /*------------------------------------*/
    const prm =
    {
        "alf_pro": alf
    }
    /*------------------------------------*/
    /* Assemblaggio parametri             */
    /*------------------------------------*/
    const dat = $.param(prm)              ;
    /*------------------------------------*/
    /* Lettura                            */
    /*------------------------------------*/
    $.ajax({ type: 'POST',
              url: "./elebfop0",
      contentType: "application/x-www-form-urlencoded",
             data: dat,
          success: function(data)
          {
              /*--------------------------*/
              /* Esito lettura            */
              /*--------------------------*/
              let res = data.substring(0, 2);
          } 
    });    
    /*------------------------------------*/
    /* Variabile in uscita                */
    /*------------------------------------*/
    return res                            ;
/*----------------------------------------*/
/* Lettura codice prodotto - fine         */
/*========================================*/
}

/*========================================*/
/* Ricerca codice prodotto nei documenti  */
/* aperti                                 */
/*----------------------------------------*/
function ric_pro(alf)
{
    /*------------------------------------*/
    /* Test su codice prodotto            */
    /*------------------------------------*/
    if (alf == ''){return;}
    /*------------------------------------*/
    /* Ricerca prodotto                   */
    /*------------------------------------*/
    $.ajax({
        type: "POST",
        url: "./elebfop1",
        contentType: "application/x-www-form-urlencoded",
               data: "rsp_doc="  + rsp_doc +
                      "&alf_pro=" + alf,
        /*--------------------------------*/
        /* Esito lettura                  */
        /*--------------------------------*/
        success: function (data)
        {
            /*----------------------------*/
            /* Normalizzazione valore     */
            /*----------------------------*/
            var exi = data.trim()         ;
            /*----------------------------*/
            /* Se non trovato nessun do-  */
            /* cumento                    */
            /*----------------------------*/
            if (exi == 'ND')
            {
                alert ("Nessun documento aperto per il prodotto!");
            }
             else if (exi == 'NE')
            {
                alert ("Codice prodotto non trovato!");
            }
             else
            /*----------------------------*/
            /* Se trovato documenti       */
            /*----------------------------*/
            {
                $("#dialog").html(data);
                $("#dialog").dialog("option","title","Documenti di carico").dialog('open');
            }
        }
    });

/*----------------------------------------*/
/* Ricerca codice prodotto - fine         */
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
/* Funzione di pulizia stringa            */
/*                                        */
/* N.B.: usata per togliere caratteri non */
/*       alfabetici o numerici            */
/*----------------------------------------*/
function pul_str(inp_str)
{
    return inp_str.replace(/[^a-zA-Z0-9]/g, '');
};    
    