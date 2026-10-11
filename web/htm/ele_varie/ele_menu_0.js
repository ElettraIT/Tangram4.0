/*========================================*/
/* ele_menu_0 - Javascript - by CharGPT   */
/*                                        */
/* PROVE MENU'                            */
/* -------------------------------------- */
/* Aggiornamento   : 09/05/2025           */
/*========================================*/

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Al click sull'icona, nasconde o    */
    /* mostra solo i testi del menu       */
    /*------------------------------------*/
    $('.menu-toggle').click(function () {
        $('.sidebar').toggleClass('minimized');
    });

    /*====================================*/
    /* Cambio contenuto iFrame            */
    /*------------------------------------*/
    $('.sidebar a').click(function (e) {
        e.preventDefault();
        const src = $(this).data('src');
        $('#main-frame').attr('src', src);
    });

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/
});
