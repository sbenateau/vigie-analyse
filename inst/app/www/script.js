$( document ).ready(function() {
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
});
