HTMLWidgets.widget({

  name: 'phosphorr',

  type: 'output',

  factory: function(el, width, height) {

    var dock = null;

    return {

      renderValue: function(opts) {

        if (dock === null) {
  	      // Create DockPanel
  	      dock = new phosphorjs.DockPanel();

  	      // Create ContentWidget (right now, uses predefined function)
  	      // var r1 = new phosphorjs.ContentWidget('Red');
  	      // var r2 = new phosphorjs.ContentWidget('Blue');

  	      // Add widgets in appropriate places - this can be done in any order
  	      // dock.addWidget(r1);
  	      // dock.addWidget(r2);

  	      // Attach BoxPanel to el
  	      phosphorjs.Widget.attach(dock, $('#'+el.id).find(".phosphorr-shim")[0]);
        }

        // http://ianjgough.com/jquery/add-and-remove-stylesheets-with-jquery/
        // Smartify this later - How do I know the 2nd is flexdash???
        // $("head").find("link").attr("rel", "stylesheet")[0].id = "tmpl-phosphor";
        // $("head").find("link").attr("rel", "stylesheet")[1].id = "tmpl-flexdash";

        // $("#tmpl-phosphor").remove();
        // $("#tmpl-flexdash").remove(); // +1-1 = 0.  Keeping here for reference.
      },

      resize: function(width, height) {
        dock.update();
      },

      // Give access to box if anyone needs it on the outside
      // Using in console:  HTMLWidgets.find(".phosphorr").getBox()
      getDock: function() {
        return dock;
      }
    };
  }
});

// Helper function to get an existing phosphorr object via the htmlWidgets object.
function getDock(id) {

  // Get the HTMLWidgets object
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  var phosphorrObj = null;
  if( typeof(htmlWidgetsObj) !== "undefined"){
    // Use the getBox method we created to get the underlying gridstack
    phosphorrObj = htmlWidgetsObj.getDock();
  }

  return(phosphorrObj);
}

// ---- R -> Javascript

// Note:  Might want to make widget ids boxID + widgetID so can have same widgetID in different stacks.  Right now, based on best practices, items must have unique IDs, even across different boxes

// Custom handler to add a new widget
Shiny.addCustomMessageHandler('phosphorr:addWidget', function(message) {
  // Add widget content to DOM
  $('#'+message.dockID).append(message.content);

  // Create widget and bind content
  var steve = new phosphorjs.Widget({node: document.getElementById(message.widgetID)});

  // Add title and make closable
  steve.title.label = message.title;
  // steve.title.closable = message.closable;

  // Need to rebind Shiny on certain events (for now, show and resize only)
  // Also need throttling for resize:  https://shiny.rstudio.com/articles/js-dashboard.html
  steve.onAfterShow = function(msg) { Shiny.bindAll(this); };
  steve.onResize = _.throttle( function(msg) { Shiny.bindAll(this); }, 250 );

  // Attach widget to panel (first widget is dock)
  getDock(message.dockID).addWidget(widget = steve, options = {mode: 'tab-after'});
});
