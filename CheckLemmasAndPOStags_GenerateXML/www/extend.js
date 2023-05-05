shinyjs.getSize = function()
{
  function go()
  {
    Shiny.onInputChange("width" , window.innerWidth );
    Shiny.onInputChange("height", window.innerHeight);
  };

  go();
  window.addEventListener('resize', go);
}
