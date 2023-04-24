/* Make textAreaInput auto-resizing the text box  */

var observe;

if (window.attachEvent) 
{
  observe = function (element, event, handler) 
  {
    element.attachEvent('on'+event, handler);
  };
}
else 
{
  observe = function (element, event, handler) 
  {
    element.addEventListener(event, handler, false);
  };
}

function init2 (inputID) 
{
  var text = document.getElementById(inputID);

  text.style.minHeight = '32px';
//text.style.minHeight = '52px';
  
  function resize () 
  {
    text.style.height = 'auto';
    text.style.height = text.scrollHeight+'px';
  }
  
  /* 0-timeout to get the already changed text */
  
  function delayedResize () 
  {
    window.setTimeout(resize, 0);
  }
  
  observe(text, 'click',   resize);
  observe(text, 'change',  resize);
  observe(text, 'cut',     delayedResize);
  observe(text, 'paste',   delayedResize);
  observe(text, 'drop',    delayedResize);
  observe(text, 'keydown', delayedResize);

  text.focus();
  text.select();
  resize();
  
  text.style.overflow = 'hidden';
}

init2('artAuteur');
init2('artTitel');
init2('artOpmerkingen');
