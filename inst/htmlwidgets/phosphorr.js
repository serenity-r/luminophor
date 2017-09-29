HTMLWidgets.widget({

  name: 'phosphorr',

  type: 'output',

  factory: function(el, width, height) {

    var box = null;

    return {

      renderValue: function(opts) {

        if (box === null) {
          // Create BoxPanel
  	      box = new phosphorjs.BoxPanel({ direction: 'left-to-right', spacing: 0 });

  	      // Create DockPanel
  	      var dock = new phosphorjs.DockPanel();
  	      box.addWidget(dock);

  	      // Create ContentWidget (right now, uses predefined function)
  	      // var r1 = new phosphorjs.ContentWidget('Red');
  	      // var r2 = new phosphorjs.ContentWidget('Blue');

  	      // Add widgets in appropriate places - this can be done in any order
  	      // dock.addWidget(r1);
  	      // dock.addWidget(r2);

  	      // Attach BoxPanel to el
  	      phosphorjs.Widget.attach(box, $('#'+el.id).find(".phosphorr-shim")[0]);
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

// ---- R -> Javascript

// Note:  Might want to make widget ids boxID + widgetID so can have same widgetID in different stacks.  Right now, based on best practices, items must have unique IDs, even across different boxes

// Custom handler to add a new widget
Shiny.addCustomMessageHandler('phosphorr:addWidget', function(message) {
  // Add widget content to DOM
  $('#'+message.boxID).append(message.content);

  // Create widget and bind content
  var steve = new phosphorjs.Widget({node: document.getElementById(message.widgetID)});

  // Add title and make closable
  steve.title.label = message.title;
  steve.title.closable = message.closable;

  // Attach widget to panel (first widget is dock)
  getBox(message.boxID).widgets[0].addWidget(widget = steve, options = {mode: 'tab-after'});
});
