var width = 0;

$(document).on("shiny:connected", function(e) 
{
  height = window.innerHeight;
  Shiny.onInputChange("height", height);
});

$(window).resize(function(e) 
{
  height = window.innerHeight;
  Shiny.onInputChange("height", height);
});
