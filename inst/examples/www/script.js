// Credits: trestletech
// https://github.com/trestletech/ShinyChat/blob/master/sendOnEnter.js

// Sending on ENTER
jQuery(document).ready(function(){
  jQuery('#message_field').keypress(function(evt){
    if (evt.keyCode == 13){
      // Enter, simulate clicking send
      jQuery('#send').click();
    }
  });
})

// Scrolling down
var oldContent = null;
window.setInterval(function() {
  var elem = document.getElementById('chatbox');
  if (oldContent != elem.innerHTML){
    scrollToBottom();
  }
  oldContent = elem.innerHTML;
}, 300);

function scrollToBottom(){
  var elem = document.getElementById('chatbox');
  elem.scrollTop = elem.scrollHeight;
}
