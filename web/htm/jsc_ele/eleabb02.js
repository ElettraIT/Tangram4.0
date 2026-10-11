/*========================================*/
/* eleabb01 - Javascript - N&A            */
/*                                        */
/* Elettra : Messa a scaffale             */
/* -------------------------------------- */
/* Aggiornamento   : 14/03/2025           */
/*========================================*/

/*========================================*/
/* Nome del modulo form                   */
/*----------------------------------------*/
mod_frm = "#bfo_002"                      ;
/*========================================*/
/* Indirizzo per lettura IP               */
/*----------------------------------------*/
url_rsp = "./getip.php"                   ;
/*----------------------------------------*/
/* Indirizzo per espansione ubicazioni    */
/*----------------------------------------*/
url_exu = "../cgi-bin/eleodsg0"           ;
/*----------------------------------------*/
/* Indirizzo per controllo ubicazione     */
/*----------------------------------------*/
url_ctu = "../cgi-bin/eleodsu0"           ;
/*----------------------------------------*/
/* Indirizzo per scrittura bfo            */
/*----------------------------------------*/
url_rc1 = "../cgi-bin/eleabbX1"           ;
/*----------------------------------------*/
/* Indirizzo per scrittura bfo            */
/*----------------------------------------*/
url_rc2 = "../cgi-bin/eleubix0"           ;
/*----------------------------------------*/
/* Indirizzo per lettura prodotto         */
/*----------------------------------------*/
url_pro = "../cgi-bin/elebfop0"           ;
/*----------------------------------------*/
/* Indirizzo per stampa documento         */
/*----------------------------------------*/
url_stp = "../cgi-bin/elebfo1s"           ;

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
    /* - '1' : Creazione lista prodotti   */
    /* - '2' : Messa a scaffale           */
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
        liv_001 ()                        ;
    }
    else if (liv_ope == 2)
    {
        /*--------------------------------*/
        /* Livello '2'                    */
        /*--------------------------------*/
        liv_002 ()                        ;
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
        url : url_rsp,
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
            var des = $('#rsp_doc option:selected').text();
            $('#rsp_des').val((des.trim()));
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
/* Creazione lotto                        */
/*----------------------------------------*/
function liv_001 ()
{
    /*====================================*/
    /* Rimuovo i binding degli eventi     */
    /* per evitare duplicazioni           */
    /*------------------------------------*/
    $('#alf_pro').off('change')           ;
    $("#res").off('click')                ;
    $("#but_cnf").off('click')            ;
    $("#alf_pro").select()                ;
    $("#alf_pro").focus()                 ;
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
    /* Responsabile                       */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    rsp_des = $('#rsp_des').val()         ;

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
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Tabella di riferimento         */
        /*--------------------------------*/
        var tbody = $('#pro tbody')       ;
        /*--------------------------------*/
        /* Svuoto e nascondo              */
        /*--------------------------------*/
        tbody.empty()                     ;
        $('#listaProdotti').hide()        ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });
    /*====================================*/
    /* Controllo su campo prodotto per    */
    /* evitare il submit automatico       */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#alf_pro').on('keydown', function(e) {
        if (e.which === 13) {
            /*----------------------------*/
            /* Previene funzionamento     */
            /* normale e triggera change  */
            /*----------------------------*/
            e.preventDefault();
            $(this).trigger('change');
        }
    });
    
    /*====================================*/
    /* Cambio Prodotto                    */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#alf_pro').on('change',function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#msg').html('')                ;
        /*--------------------------------*/
        /* Valori per chiamata            */
        /*--------------------------------*/
        var alf_pro = $(this).val()
                             .trim()
                             .toUpperCase();
        var tip_ope = "LP"                ;
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm = {
            "tip_ope": tip_ope,
            "alf_pro": alf_pro,
        };
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm);
        /*================================*/
        /* Chiamata ajax per ricerca con  */
        /* Barcode                        */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url : url_rc1,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            success: function(rec_pro) {
                ric_pro(rec_pro)          ;
            },
            error: function(xhr, status, error) {
                console.error("Errore nella chiamata AJAX: ", error);
            }
        });
    });

    /*====================================*/
    /* Bottone conferma                   */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#but_cnf").click(function()
    {
        var rsp_lot = $('#rsp_des').val();
        /*--------------------------------*/
        /* Estrazione valori delle righe  */
        /*--------------------------------*/
        var prodotti = [];
        $("#pro tbody tr").each(function() {
            var alf_pro = $(this).find("#alf_pro").text().trim();
            var qta_rig = $(this).find("#qta_rig").text().trim();
            if (alf_pro) {
                prodotti.push({
                    alf_pro: alf_pro,
                    rsp_lot: rsp_lot,
                    qta_rig: qta_rig
                });
            }
        });
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        if (prodotti.length === 0) {
            $('#msg').addClass('msg_red')
            .html('<h1>NESSUN PRODOTTO SELEZIONATO</h1>');            
            return;
        }
        /*--------------------------------*/
        /* Controllo esistenza lotto      */
        /* selezionato                    */
        /*--------------------------------*/
        num_prg = $('#num_prg').val()     ;
        if (num_prg !== '')
        {
            /*----------------------------*/
            /* Al livello successivo      */
            /*----------------------------*/
            $('#liv_ope').val('2')        ;
            $(mod_frm).submit()           ;
            return;
        }
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url: "insertLotti.php",
            data: { prodotti: prodotti },
            dataType: "json",
            success: function(response) {
                $('#num_prg').val(response.num_prg);
                /*------------------------*/
                /* Al livello successivo  */
                /*------------------------*/
                $('#liv_ope').val('2') ;
                $(mod_frm).submit()    ;
            },
            error: function(xhr, status, error) {
            }
        });
    });
    /*====================================*/
    /* Selezione lotto aperto             */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#num_lot').change(function()
    {
        /*--------------------------------*/
        /* Recupero il valore selezionato */
        /*--------------------------------*/
        var num_prg = $(this).val();
        /*--------------------------------*/
        /* Chiamata AJAX per recupero     */
        /* prodotti del lotto             */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url: "getProdotti.php",
            data: { num_prg: num_prg },
            dataType: "json",
            /*----------------------------*/
            /* Gestione risposta AJAX     */
            /*----------------------------*/
            success: function(response) {
                if (response.status === "success") 
                {
                    /*--------------------*/
                    /* Popolamento tabella*/
                    /* prodotti           */
                    /*--------------------*/
                    pop_tab(response.prodotti);
                    /*--------------------*/
                    /* Visualizzazione    */
                    /* tabella            */
                    /*--------------------*/
                    $('#listaProdotti').show();
                    $('#num_prg').val(num_prg);
                } 
                else 
                {
                    /*--------------------*/
                    /* Gestione errore    */
                    /* lato server        */
                    /*--------------------*/
                    alert("Errore: " + response.message);
                }
            },
            /*----------------------------*/
            /* Gestione errore AJAX       */
            /*----------------------------*/
            error: function(xhr, status, error) {
                console.error("Errore AJAX:", error);
                alert("Errore durante il recupero dei prodotti del lotto.");
            }
        });
    });          
}

/*----------------------------------------*/
/* Fine funzione livello '1'              */
/*========================================*/

/*========================================*/
/* Funzione di gestione livello '2'       */
/*                                        */
/* Messa a scaffale                       */
/*----------------------------------------*/
function liv_002 ()
{
    /*====================================*/
    /* Rimuovo i binding degli eventi     */
    /* per evitare duplicazioni           */
    /*------------------------------------*/
    $('#alf_pro').off('change')           ;
    $("#res").off('click')                ;
    $("#but_cnf").off('click')            ;
    $("#acc_ubi").off('change')           ;
    $("#alf_pro").select()                ;
    $("#alf_pro").focus()                 ;
    $("#acc_ubi").on('change', ctr_ubi)   ;
    /*====================================*/
    /* Numero Progressivo                 */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    num_prg = $('#num_prg').val()         ;
    /*------------------------------------*/
    /* Chiamata AJAX per recupero         */
    /* prodotti del lotto                 */
    /*------------------------------------*/
    $.ajax({
        type: "POST",
        url: "getProdotti.php",
        data: { num_prg: num_prg },
        dataType: "json",
        /*--------------------------------*/
        /* Gestione risposta AJAX         */
        /*--------------------------------*/
        success: function(response) {
            if (response.status === "success") 
            {
                /*------------------------*/
                /* Assegnaione prodotti   */
                /*------------------------*/
                prodotti = response.prodotti;
                /*------------------------*/
                /* Informazioni per       */
                /* prodotto               */
                /*------------------------*/
                inf_pro(prodotti)         ;
            } 
            else 
            {
                /*------------------------*/
                /* Gestione errore        */
                /* lato server            */
                /*------------------------*/
                alert("Errore: " + response.message);
            }
        },
        /*--------------------------------*/
        /* Gestione errore AJAX           */
        /*--------------------------------*/
        error: function(xhr, status, error) {
            console.error("Errore AJAX:", error);
            alert("Errore durante il recupero dei prodotti del lotto.");
        }
    });
    /*====================================*/
    /* Bottone reset                      */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $("#res").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });
    /*====================================*/
    /* Cambio Prodotto                    */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Valori per chiamata            */
        /*--------------------------------*/
        var alf_pro = $(this).val()
                             .trim()
                             .toUpperCase();
        sel_pro(alf_pro)                  ;
    });

    /*====================================*/
    /* Gestione click nella cella con     */
    /* quantità da inserire               */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#pro').on('click', '#cel_qtu', function() {
        /*--------------------------------*/
        /* Recupero il valore             */
        /*--------------------------------*/
        var cel_val = $(this).text().trim();
        /*--------------------------------*/
        /* Lo assegno al campo input      */
        /*--------------------------------*/
        if(cel_val !== "")
        {
            $('#ubi_qta').val(cel_val)    ;
            /*----------------------------*/
            /* Seleziono campo ubicazione */
            /*----------------------------*/
            $('#acc_ubi').select()        ;
            $('#acc_ubi').focus()         ;
        }
    });

    /*====================================*/
    /* Bottone conferma                   */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $("#but_cnf").click(function()
    {
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val() ;
        var alf_pro = $('#alf_pro').val()
                                   .toUpperCase();
        var ubi_qta = $('#ubi_qta').val() ;
        var acc_ub1 = "LTRA";
        var acc_ub2 = $('#acc_ubi').val().trim();
        var num_prg = $('#num_prg').val() ;

        /*--------------------------------*/
        /* Trova la riga corrispondente in*/
        /* #listaLotto basata sul campo   */
        /* "alf_pro2"                     */
        /*--------------------------------*/
        var $row = $('#listaLotto tbody tr').filter(function() {
            var cellVal = $(this).find('#alf_pro2').text().trim().toUpperCase();
            return cellVal === alf_pro;
        });
        /*--------------------------------*/
        /* Se la riga è trovata, estrae il*/
        /* campo hidden con la lista delle*/
        /* ubicazioni (se esistente)      */
        /*--------------------------------*/
        if ($row.length > 0) {
            var lst_ubi = $row.find('input[name="lst_ubi"]').val();
        }    
        /*--------------------------------*/
        /* Controllo se campo non vuoto   */
        /*--------------------------------*/
        if (acc_ub2 == '')
        {
            $('#acc_ubi').val('')     ;
            $('#msg').addClass('msg_red')
                    .html("<h1>INSERIRE UBICAZIONE</h1>");
            $('#acc_ubi').select()    ;
            $('#acc_ubi').focus()     ;
            return                    ;
        }
        else if(!lst_ubi.includes(acc_ub2.toUpperCase()))
        {
        /*--------------------------------*/
        /* Messaggio inserimento nuova ubi*/
        /*--------------------------------*/
        cnf_dlg("Vuoi inserire una nuova ubicazione per questo prodotto?", 
            function() {
                /*------------------------*/
                /* Funzione con_frm       */
                /*------------------------*/
                /*------------------------*/
                /* Tipo operazione        */
                /*------------------------*/
                var tip_ope = "MS"        ;
                /*------------------------*/
                /* Oggetto con i Parametri*/
                /*------------------------*/
                const prm =
                {
                    "tip_ope": tip_ope,
                    "rsp_doc": rsp_doc,
                    "alf_pro": alf_pro,
                    "ubi_qta": ubi_qta,
                    "acc_ub1": acc_ub1,
                    "acc_ub2": acc_ub2
                }
                /*------------------------*/
                /* Assemblaggio parametri */
                /*------------------------*/
                const dat = $.param(prm)  ;
                upd_tng(dat);
                /*------------------------*/
                /* Oggetto con i Parametri*/
                /*------------------------*/
                const prm1 =
                {
                    "num_prg": num_prg,
                    "alf_pro": alf_pro,
                    "ubi_qta": ubi_qta
                }
                /*------------------------*/
                /* Assemblaggio parametri */
                /*------------------------*/
                const dat1 = $.param(prm1);
                upd_dtb(dat1);

            },
            function() {
                /*------------------------*/
                /* Funzione can_cel       */
                /*------------------------*/
                $('#acc_ubi').val('')     ;
                $('#acc_ubi').focus()     ;
                return;
            }
        );
        }
        else
        {
            /*------------------------*/
            /* Cambio ubicazione      */
            /*------------------------*/
            /*------------------------*/
            /* Tipo operazione        */
            /*------------------------*/
            var tip_ope = "MS"        ;
            /*------------------------*/
            /* Oggetto con i Parametri*/
            /*------------------------*/
            const prm =
            {
                "tip_ope": tip_ope,
                "rsp_doc": rsp_doc,
                "alf_pro": alf_pro,
                "ubi_qta": ubi_qta,
                "acc_ub1": acc_ub1,
                "acc_ub2": acc_ub2
            }
            /*------------------------*/
            /* Assemblaggio parametri */
            /*------------------------*/
            const dat = $.param(prm)  ;
            upd_tng(dat);
            /*------------------------*/
            /* Oggetto con i Parametri*/
            /*------------------------*/
            const prm1 =
            {
                "num_prg": num_prg,
                "alf_pro": alf_pro,
                "ubi_qta": ubi_qta
            }
            /*------------------------*/
            /* Assemblaggio parametri */
            /*------------------------*/
            const dat1 = $.param(prm1);
            upd_dtb(dat1);

        }

    });    
    /*====================================*/
    /* Associazione evento click sul TD   */
    /* alf_pro per eseguire la funzione   */
    /* sel_pro                            */
    /*------------------------------------*/
    $(document).on('click', '#listaLotto td#alf_pro2', function(e) {
        /*--------------------------------*/
        /* Estrae il valore del codice    */
        /* prodotto dal td                */
        /*--------------------------------*/
        var alf_pro = $(this).text().trim();
        /*--------------------------------*/
        /* Chiama la funzione sel_pro     */
        /* con il codice estratto         */
        /*--------------------------------*/
        sel_pro(alf_pro)                  ;
    });

    /*====================================*/
    /* Bottone refresh                    */
    /*------------------------------------*/
    $('#but_ref').click(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });
}

/*----------------------------------------*/
/* Fine funzione livello '2'              */
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

/*========================================*/
/* Funzione di pulizia stringa            */
/*                                        */
/* N.B.: usata per togliere caratteri non */
/*       alfabetici o numerici            */
/*----------------------------------------*/
function pul_str(inp_str)
{
    return inp_str.replace(/[^a-zA-Z0-9]/g, '');
}

/*========================================*/
/* Funzione per rendere numero riga con   */
/* 0 iniziali                             */
/*----------------------------------------*/
function pad(num, size) 
{
    var s = String(num);
    while(s.length < size) s = "0" + s;
    return s;
}
/*========================================*/
/* Funzione per pulire le chiavi di un    */
/* oggetto                                */
/*----------------------------------------*/
function trimKeys(obj) 
{
    var newObj = {};
    Object.keys(obj).forEach(function(key) {
        newObj[key.trim()] = obj[key];
    });
    return newObj;
}

/*========================================*/
/* Funzione per disabilitare campi input  */
/* delle righe gia spuntate               */
/*----------------------------------------*/
function dis_inp() 
{
    /*------------------------------------*/
    /* Passo in rassegna ogni riga        */
    /*------------------------------------*/
    $('#rig').find('input[type="checkbox"]').each(function() {
        /*--------------------------------*/
        /* Se il checkbox è spuntato,     */
        /* disabilita l'input quantità    */
        /* corrispondente                 */
        /*--------------------------------*/
        if ($(this).is(':checked')) {
            $(this).closest('tr').find('#qtv').prop('disabled', true);
        }
    });
}

/*========================================*/
/* Funzione per ordinare ed evidenziare   */
/* la tabella in base al codice prodotto  */
/*----------------------------------------*/
function hig_rig() {
    /*------------------------------------*/
    /* Legge il valore inserito nel campo */
    /* di ricerca prodotto                */
    /*------------------------------------*/
    var alf_pro = $('#alf_pro').val().trim().toUpperCase();
    /*------------------------------------*/
    /* Se nessun codice ripristiniamo     */
    /* ordine originale deselezionando    */
    /*------------------------------------*/
    if (alf_pro === "") {
        $('#rig tbody tr').removeClass('sel');
        return;
    }
    /*------------------------------------*/
    /* Seleziona tutte le righe del tbody */
    /* della tabella con id "rig"         */
    /*------------------------------------*/
    var $rows = $('#rig tbody tr');
    /*------------------------------------*/
    /* Inizializzo array per divisione    */
    /* righe selezionate e non            */
    /*------------------------------------*/
    var rig_sel = [];
    var non_sel = [];
    
    /*------------------------------------*/
    /* Itera su ogni riga                 */
    /*------------------------------------*/
    $rows.each(function() {
        var $row = $(this);
        /*--------------------------------*/
        /* Recupera il contenuto della    */
        /* cella del codice prodotto      */
        /*--------------------------------*/
        var cod_row = $row.find('#alf').val() || $row.find('#alf').text();
        cod_row = cod_row.trim();
        /*--------------------------------*/
        /* Se corrisponde lo aggiunge     */
        /* all'array e lo seleziona       */
        /*--------------------------------*/
        if (cod_row === alf_pro) 
        {
            $row.addClass("sel");
            rig_sel.push($row);
        }
        /*--------------------------------*/
        /* Altrimenti rimuove la selezione*/
        /* e aggiunge alll'array dei non  */
        /* selezionati                    */
        /*--------------------------------*/
        else
        {
            $row.removeClass("sel"); 
            non_sel.push($row);
        }
    });
    
    /*------------------------------------*/
    /* Ordina l'array in base a quello    */
    /* che vogliamo (qui per progressivo) */
    /*------------------------------------*/
    rig_sel.sort(function(a, b) {
        return parseInt($(a).attr('id').match(/\d+/)[0]) - parseInt($(b).attr('id').match(/\d+/)[0]);
    });
    non_sel.sort(function(a, b) {
        return parseInt($(a).attr('id').match(/\d+/)[0]) - parseInt($(b).attr('id').match(/\d+/)[0]);
    });
    /*------------------------------------*/
    /* Svuota il tbody e reinserisce le   */
    /* righe prima evidenziate            */
    /*------------------------------------*/
    var $tbody = $('#rig tbody');
    $tbody.empty();
    rig_sel.forEach(function($r) 
    {
        $tbody.append($r);
    });
    non_sel.forEach(function($r) 
    {
        $tbody.append($r);
    });
    /*------------------------------------*/
    /* Selezione e focus sul campo        */
    /* prodotto                           */
    /*------------------------------------*/
    $('#alf_pro').focus()                 ;
    $('#alf_pro').select()                ;
}

/*========================================*/
/* Funzione controllo check totali        */
/*----------------------------------------*/
function chk_tot() 
{
    /*====================================*/
    /* Seleziona tutte le checkobox sia   */
    /* spuntate che non e controlla se    */
    /* il documento è da chiudere         */
    /*------------------------------------*/
    var tot_chk = $('input[type="checkbox"]').length;
    var chk_chk = $('input[type="checkbox"]:checked').length;

    if (tot_chk === chk_chk)
    {
        cel_css("stp", "G")               ;
        $('#stp').prop("disabled",false)    ;
        $('#alf_pro').prop("disabled",true);
    }
}

/*========================================*/
/* Funzione per popolazione tabella       */
/* creazione lotto                        */
/*----------------------------------------*/
function pop_tab(prodotti) {
    /*------------------------------------*/
    /* Tabella su cui agire               */
    /*------------------------------------*/
    var tbody = $('#listaProdotti tbody');
    /*------------------------------------*/
    /* Svuoto il tbody                    */
    /*------------------------------------*/
    tbody.empty();
    /*------------------------------------*/
    /* Creazione righe                    */
    /*------------------------------------*/
    prodotti.forEach(function(prodotto) {
        if(prodotto.flg_lot !== "#")
        {
            var riga = `<tr>
                <td align="left" id="alf_pro">${prodotto.alf_pro}</td>
                <td align="left" id="qta_rig">${prodotto.qta_rig}</td>
                </tr> `;
            tbody.append(riga);
        }    
    });
}

/*========================================*/
/* Funzione per recuperare ubicazioni e   */
/* giacenze                               */
/*----------------------------------------*/
function inf_pro(prodotti)
{
    /*------------------------------------*/
    /* Inizializzazione dati ricevuti     */
    /*------------------------------------*/
    var prd_prc = 0                       ;
    var prd_ubi = []                      ;
    /*------------------------------------*/
    /* Tipo Operazione                    */
    /*------------------------------------*/
    var tip_ope = "GU"                    ;
    /*------------------------------------*/
    /* Iterazione su ogni prodotto        */
    /*------------------------------------*/
    prodotti.forEach(function(prd) {
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        var prm =
        {
            "tip_ope": tip_ope,
            "alf_pro": prd.alf_pro
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;
        $.ajax({
            type: "POST",
            url: url_rc1,
            data: dat,
            contentType: "application/x-www-form-urlencoded",
            success: function(rec) {
                if (rec) 
                {
                    /*----------------------------*/
                    /* Se il JSON arriva come     */
                    /* array prendo il primo      */
                    /* elemento                   */
                    /*----------------------------*/
                    var recObj = Array.isArray(rec) ? rec[0] : rec;
                    /*----------------------------*/
                    /* Pulisco le chiavi dell     */
                    /* oggetto                    */
                    /*----------------------------*/
                    recObj = trimKeys(recObj)     ;
                    /*----------------------------*/
                    /* Estrazione ubicazione      */
                    /* principale, indice di      */
                    /* percorso e ubicazioni extra*/
                    /*----------------------------*/
                    prd.ubi_pri = recObj.ubi_pri  ;
                    prd.inx_pri = recObj.inx_pri  ;
                    prd.ubi_ext = ""              ;
                    /*----------------------------*/
                    /* Ciclo sulle chiavi dell'   */
                    /* oggetto per estrarre la    */
                    /* giacenza della  ubicazione */
                    /* principale ed altre        */
                    /* ubicazioni extra           */
                    /*----------------------------*/
                    var keysExtra = [];
                    for (var key in recObj) {
                        if (recObj.hasOwnProperty(key)) {
                            if (key === prd.ubi_pri)
                            {
                                prd.qta_pri = recObj[key];
                            }
                            if (key !== "ubi_pri" 
                                    && key !== "inx_pri" 
                                    && key !== "cls_jsn" 
                                    && key !== prd.ubi_pri
                                    && key !== "LTRA"
                                    && key !== "LRIC"
                                    && key !== "LSPED") 
                            {
                                prd.ubi_ext += "<span style='color:red;'>"
                                + key 
                                + "</span>: " + recObj[key] 
                                + "<br>";
                            }
                            if (key !== "inx_pri" 
                                && key !== "cls_jsn"
                                && key !== "ubi_pri"
                                && key !== "LTRA"
                                && key !== "LRIC"
                                && key !== "LSPED")
                            {
                                keysExtra.push(key);
                            }
                        }
                    }
                    /*----------------------------*/
                    /* Salvo le chiavi extra in forma  */
                    /* di stringa separata da virgole */
                    /*----------------------------*/
                    prd.lista_ub = keysExtra.join(",");
                    /*----------------------------*/
                    /* Genero l'array finale      */
                    /*----------------------------*/
                    prd_ubi.push(prd);
                } 
                else 
                {
                    console.warn("Nessuna ubicazione trovata per", prd.alf_pro);
                }
                /*--------------------------------*/
                /* Controllo se tutti i dati      */
                /* sono stati recuperati          */
                /*--------------------------------*/
                prd_prc++;
                if (prd_prc === prodotti.length) {
                    /*----------------------------*/
                    /* Ordinamento per indice di  */
                    /* percorso                   */
                    /*----------------------------*/
                    prd_ubi.sort((a, b) => a.inx_pri - b.inx_pri);
                    /*----------------------------*/
                    /* Popolamento tabella        */
                    /*----------------------------*/
                    pop_lot(prd_ubi);
                }
            },
            error: function(xhr, status, error) {
                console.error("Errore AJAX ubicazioni:", error);
            }
        });
    });
}
/*========================================*/
/* Funzione per popolazione tabella       */
/* con indice di percorso                 */
/*----------------------------------------*/
function pop_lot(prodotti)
{
    /*------------------------------------*/
    /* Pulizia della tabella              */
    /*------------------------------------*/
    var tbody = $("#listaLotto tbody");
    tbody.empty();
    var row = false;
    /*------------------------------------*/
    /* Aggiunta delle nuove righe         */
    /*------------------------------------*/
    prodotti.forEach(function(prd) {
        if(prd.flg_lot !== "#")
        {        
            var riga = `<tr>
                            <td align="left" id="alf_pro2">${prd.alf_pro}</td>
                            <td align="right" id="qta_rig2">${prd.qta_rig}</td>
                            <td align="left" id="ubi_pri2" style="color:red;">${prd.ubi_pri}</td>
                            <td align="right" id="qta_pri2">
                                ${typeof prd.qta_pri !== 'undefined' ? prd.qta_pri : '0'}
                            </td>
                            <td align="left" id="ubi_ext2">${prd.ubi_ext}</td>
                            <input type="hidden" name="inx_pri2" id="inx_pri2" value="${prd.inx_pri}">
                            <input type="hidden" name="lst_ubi" id="lst_ubi" value="${prd.lista_ub}">
                        </tr>`;
            tbody.append(riga);
            row = true;
        }
    });
    /*------------------------------------*/
    /* Mostro la tabella se presente      */
    /* almeno una riga                    */
    /*------------------------------------*/
    $('#listaLotto').toggle(!!row);
}
/*========================================*/
/* Funzione per effettuare movimenti      */
/* magazzino lato server                  */
/*----------------------------------------*/
function upd_tng(dat) {
    /*------------------------------------*/
    /* Aggiornamento Tangram              */
    /* lato server                        */
    /*------------------------------------*/
    $.ajax({
        /*--------------------------------*/
        /* Richiamo modulo                */
        /*--------------------------------*/
        type: "POST",
        url: url_rc2,
        contentType: "application/x-www-form-urlencoded",
        data: dat,
        /*--------------------------------*/
        /* Se andato a buon fine          */
        /*--------------------------------*/
        success: function (data)
        {
            /*----------------------------*/
            /* Normalizzazioni            */
            /*----------------------------*/
            $('#alf_pro').val('')     ;
            $('#ubi_qta').val('')     ;
            $('#acc_ubi').val('')     ;
            $('#cel_qtu').text('')    ;
        }
    });
}

/*========================================*/
/* Funzione per aggiornare database con   */
/* le nuove quantità                      */
/*----------------------------------------*/
function upd_dtb(dat1) {
    /*------------------------------------*/
    /* Aggiornamento Tangram              */
    /* lato database                      */
    /*------------------------------------*/
    $.ajax({
        /*--------------------------------*/
        /* Richiamo modulo                */
        /*--------------------------------*/
        type: "POST",
        url:"updateLotti.php",
        dataType: "json",
        data: dat1,
        /*--------------------------------*/
        /* Se andato a buon fine          */
        /*--------------------------------*/
        success: function (res)
        {
            if(res.status === "success") 
            {
                liv_002();
            }
            else
            {
                $('#msg').addClass('msg_red');
                $('#msg').html('<h1>'+ res.message.toUpperCase() +'</h1>');
            }
        },
        error: function(xhr, status, error) {
            console.error("Errore AJAX info lotto:", error);
            alert("Errore durante il recupero delle informazioni aggiornate del lotto.");
        }
    });
}
/*========================================*/
/* Funzione per controllare l'ubicazione  */
/* inserita                               */
/*----------------------------------------*/
function ctr_ubi() {
    /*--------------------------------*/
    /* Pulisco eventuali messaggi     */
    /*--------------------------------*/
    $('#msg').html('')                ;
    /*--------------------------------*/
    /* Lista ubicazioni               */
    /*--------------------------------*/
    /*--------------------------------*/
    /* Valore campo input pulito      */
    /*--------------------------------*/
    var acc_ubi = $('#acc_ubi').val()
                                .toUpperCase()
                                .trim();
        acc_ubi = pul_str (acc_ubi)   ;
    $('#acc_ubi').val(acc_ubi)        ;
    /*--------------------------------*/
    /* Se codice uguale a ubicazione  */
    /* di transito                    */
    /*--------------------------------*/
    if (acc_ubi == "LTRA")
    {
        $('#acc_ubi').val('')         ;
        $('#msg').addClass('msg_red')
                .html("<h1>UBICAZIONE NON VALIDA</h1>");
        $('#acc_ubi').select()        ;
        $('#acc_ubi').focus()         ;
        return                        ;
    };
    /*--------------------------------*/
    /* Lettura                        */
    /*--------------------------------*/
    $.ajax({ type: 'POST',
                url: url_ctu,
        contentType: "application/x-www-form-urlencoded",
                data: {cod_ubi : acc_ubi},
            success: function(data)
            {
            /*------------------------*/
            /* Esito lettura          */
            /*------------------------*/
            let res = data.substring(0, 2);
            /*------------------------*/
            /* Messaggio di errore    */
            /*------------------------*/
            if (res != 'OK')
            {
                $('#msg').addClass('msg_red')
                .html("<h1>Ubicazione di DESTINAZIONE inesistente</h1>");
                $('#acc_ubi').val('') ;
                $('#acc_ubi').select();
                $('#acc_ubi').focus() ;       
            }
        } 
    });    
}

/*========================================*/
/* Funzione per ricerca prodotto in LTRA  */
/*----------------------------------------*/
function ric_pro(rec_pro) {
        /*--------------------------------*/
        /* Tabella di riferimento         */
        /*--------------------------------*/
        var tbody = $('#pro tbody')       ;
        /*--------------------------------*/
        /* Variabile di contorllo per     */
        /* presenza prodotto              */
        /*--------------------------------*/
        var presente = false              ;
        /*--------------------------------*/
        /* Eseguo un ciclo per            */
        /* controllare in ogni riga       */
        /* se presente il prodotto        */
        /*--------------------------------*/
        $('#pro tbody td#alf_pro').each(function() {
            if ($(this).text()
                       .trim()
                       .toUpperCase() === rec_pro.trim()
                                                 .toUpperCase()) {
                presente = true;
                return false;
            }
        });

        if (presente) {
            /*----------------------------*/
            /* Prodotto già presente      */
            /*----------------------------*/
            $('#msg').addClass('msg_big')
            .html('<h1>PRODOTTO GIÀ PRESENTE NEL LOTTO</h1>');
            $('#alf_pro').val('');
            $('#alf_pro').select();
            $('#alf_pro').focus();
            return;
        }

        /*--------------------------------*/
        /* Valori per la chiamata         */
        /*--------------------------------*/
        var tip_ope = "PT"                ;
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "tip_ope": tip_ope,
            "alf_pro": rec_pro.trim()
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;
        /*================================*/
        /* Ajax per ricevere dati da      */
        /* Tangram                        */
        /*--------------------------------*/
        $.ajax({
        type: 'POST',
        contentType: "application/x-www-form-urlencoded",
        url: url_rc1,  
        data: dat,
        success: function(rec) {
            /*----------------------------*/
            /* Controllo se mi arriva     */
            /* quantità in LTRA           */
            /*----------------------------*/
            if (rec.trim() !== "" || rec.trim() === undefined)
            /*----------------------------*/
            /* Aggiungo riga alla         */
            /* tabella                    */
            /*----------------------------*/
            {
                var row =`'<tr>
                            <td id="alf_pro" align="left">${rec_pro}</td>
                            <td id="qta_rig" align="right">${rec}</td>
                          </tr>`;
                /*------------------------*/
                /* Aggiungo la riga alla  */
                /* tabella                */
                /*------------------------*/
                tbody.append(row);
                $('#listaProdotti').show();
            }
            else
            {
                $('#msg').addClass('msg_red')
                .html('<h1>PRODOTTO NON PRESENTE IN LTRA</h1>');
            }
            $('#alf_pro').val('');
            $('#alf_pro').select();
            $('#alf_pro').focus();
        },
        });
}

/*========================================*/
/* Funzione per controllare l'ubicazione  */
/* inserita                               */
/*----------------------------------------*/
function sel_pro(alf_pro) {
    /*------------------------------------*/
    /* Tabella di riferimento             */
    /*------------------------------------*/
    var tbody_tr = $('#listaLotto tbody tr');
    /*------------------------------------*/
    /* Rimuovo eventuali selezioni        */
    /*------------------------------------*/
    tbody_tr.removeClass('sel')           ;
    /*------------------------------------*/
    /* Normalizzazioni                    */
    /*------------------------------------*/
    $('#msg').html('')                    ;
    /*------------------------------------*/
    /* Tipo operazione                    */
    /*------------------------------------*/
    var tip_ope = "LP"                    ;
    /*------------------------------------*/
    /* Oggetto con i Parametri            */
    /*------------------------------------*/
    const prm = {
        "tip_ope": tip_ope,
        "alf_pro": alf_pro,
    };
    /*------------------------------------*/
    /* Assemblaggio parametri             */
    /*------------------------------------*/
    const dat = $.param(prm);
    /*====================================*/
    /* Chiamata ajax per ricerca con      */
    /* Barcode                            */
    /*------------------------------------*/
    $.ajax({
        type: "POST",
        url : url_rc1,
        contentType: "application/x-www-form-urlencoded",
        data: dat,
        success: function(rec) {
    /*------------------------------------*/
    /* Ricerca nella tabella dei          */
    /* prodotti del lotto                 */
    /*------------------------------------*/
    var found = false;
    var selectedRow = null;
    tbody_tr.each(function() 
    {
        var $row = $(this);
        var alf_pro2 = $row.find('#alf_pro2')
                                .text()
                                .toUpperCase()
                                .trim();
        if (alf_pro2 === rec.trim()) {
            found = true;
            selectedRow = $row;
            /*----------------------------*/
            /* Esco dal ciclo se          */
            /* prodotto trovato           */
            /*----------------------------*/
            return false;
        }
    });
    /*------------------------------------*/
    /* Se è stato trovato evidenzio e     */
    /* riempio le informazioni            */
    /* altrimenti messaggio di notfica    */
    /*------------------------------------*/
    if(found && selectedRow !== null)
    {
        /*--------------------------------*/
        /* Evidenzia la riga trovata      */
        /* in giallo                      */
        /*--------------------------------*/
        selectedRow.addClass('sel')   ;
        /*--------------------------------*/
        /* Estrae il valore per la        */
        /* quantità transito              */
        /*--------------------------------*/
        var qta_rig2 = selectedRow.find('#qta_rig2')
                                    .text()
                                    .trim();
        /*--------------------------------*/
        /* Inserisce il valore            */
        /* estratto nella cella all'      */
        /* interno della tabella          */
        /*--------------------------------*/
        $('#pro').find('#cel_qtu').text(qta_rig2);
        $('#ubi_qta').select()            ;
        $('#ubi_qta').focus()             ;
        $('#alf_pro').val(rec)            ;
    }
    else
    {
        /*--------------------------------*/
        /* Se il prodotto non è presente  */
        /* nel lotto                      */
        /*--------------------------------*/
        $('#msg').addClass('msg_big')
                .html("<h1>PRODOTTO NON NEL LOTTO </h1>");
        $('#alf_pro').select()            ;
        $('#alf_pro').focus()             ;
    }
        },
        error: function(xhr, status, error) {
            console.error("Errore nella chiamata AJAX: ", error);
        }
    });
}

/*========================================*/
/* Funzione creazione messaggio di dialogo*/
/* utilizza callback di conferma          */
/* o cancellazione                        */
/*----------------------------------------*/
function cnf_dlg(message, con_frm, can_cel) 
{
    /*------------------------------------*/
    /* Creazione dell'elemento div per    */
    /*  il dialogo                        */
    /*------------------------------------*/
    var $dialog = $("<div>" + message + "</div>");
    
    /*------------------------------------*/
    /* Inizializzazione del dialog        */
    /*------------------------------------*/
    $dialog.dialog({
        modal: true,                 
        title: "Attenzione",         
        resizable: false,            
        /*--------------------------------*/
        /* Definizione dei pulsanti       */
        /*--------------------------------*/
        buttons: {
            "Si": function() {
                /*------------------------*/
                /* Azione per confermare: */
                /* chiude dialog e invoca */
                /* funzione conferma      */
                /*------------------------*/
                $(this).dialog("close");
                if (typeof con_frm === "function") {
                    con_frm();
                }
            },
            "No": function() {
                /*------------------------*/
                /* Azione per cancellare: */
                /* chiude dialog e invoca */
                /* funzione cancellazione */
                /*------------------------*/
                $(this).dialog("close");
                if (typeof can_cel === "function") {
                    can_cel();
                }
            }
        },
        /*--------------------------------*/
        /* Rimozione del div              */
        /*--------------------------------*/
        close: function() {
            $(this).remove();
        }
    });
}
