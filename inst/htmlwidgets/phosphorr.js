HTMLWidgets.widget({

  name: 'phosphorr',

  type: 'output',

  factory: function(el, width, height) {

    var box = null;

    return {

      renderValue: function(opts) {

        if (box === null) {

        }

        // http://ianjgough.com/jquery/add-and-remove-stylesheets-with-jquery/
        // Smartify this later - How do I know the 2nd is flexdash???
        // $("head").find("link").attr("rel", "stylesheet")[0].id = "tmpl-phosphor";
        // $("head").find("link").attr("rel", "stylesheet")[1].id = "tmpl-flexdash";

        // $("#tmpl-phosphor").remove();
        // $("#tmpl-flexdash").remove(); // +1-1 = 0.  Keeping here for reference.
      },

      resize: function(width, height) {
        box.update();
      },

      // Give access to box if anyone needs it on the outside
      // Using in console:  HTMLWidgets.find(".phosphorr").getBox()
      getBox: function() {
        return box;
      }
    };
  }
});

// Helper function to get an existing phosphorr object via the htmlWidgets object.
function getBox(id) {

  // Get the HTMLWidgets object
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  var phosphorrObj = null;
  if( typeof(htmlWidgetsObj) !== "undefined"){
    // Use the getBox method we created to get the underlying gridstack
    phosphorrObj = htmlWidgetsObj.getBox();
  }

  return(phosphorrObj);
}
