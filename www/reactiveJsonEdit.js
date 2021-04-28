function getRandomInt(max) {
  return Math.floor(Math.random() * max);
}

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
};

async function reactiveJsonEditMod(e) {
  await sleep(1000);
  var buttonWidget = '<button id="' + e.name + "_button" + '" type="button" style="background-image: none; width: 55px;">Guardar</button>';
  var nextSelector = "#" + e.name + " div.jsoneditor div.jsoneditor-menu div.jsoneditor-modes";
  jQuery(buttonWidget).insertAfter(nextSelector);
  document.getElementById(e.name + "_button").onclick = function() {
    var listdata = HTMLWidgets.find("#" + e.name).editor.getText();
    Shiny.onInputChange(e.name + "_edit", listdata);
    Shiny.onInputChange(e.name + "_save", getRandomInt(100));
  };
};

