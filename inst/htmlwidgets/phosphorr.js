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

// This currently assumes that index of widget is related to top/left and bottom/right
function setSize(layout, widgetID, size, dir) {
  // Should always have a child at this stage
  if (layout.children !== undefined) {
    console.log("Checking children (" + layout.orientation + ")...");
    var ind = layout.children.findIndex((child) => {
      if (child.widgets !== undefined) {
        return child.widgets.some((w) => {return w.id === widgetID});
      } else if (child.children !== undefined) {
        // Recurse
        return setSize(child, widgetID, size, dir);
      } else {
        return false;
      }
    });
    console.log("...done!");

    if (ind !== undefined) {
      console.log("Found it! ind = " + ind);
      console.log("Sizes: " + layout.sizes);
      var total = layout.sizes[ind] + layout.sizes[ind+dir];
      layout.sizes[ind] = total*size;
      layout.sizes[ind+dir] = total*(1-size);
      console.log("Sizes: " + layout.sizes);
    }
  }
}

// ---- R -> Javascript

// Note:  Might want to make widget ids boxID + widgetID so can have same widgetID in different stacks.  Right now, based on best practices, items must have unique IDs, even across different boxes

// Custom handler to add a new widget
Shiny.addCustomMessageHandler('phosphorr:addWidget', function(message) {
  // Add widget content to DOM
  $('#'+message.dockID).append(message.content);

  // Create widget and bind content
  var widget = new phosphorjs.Widget({node: document.getElementById(message.widgetID)});

  // Add title and make closable
  widget.title.label = message.title;
  widget.title.caption = message.title;
  widget.title.closable = message.closable;

  // Need to rebind Shiny on certain events (for now, show and resize only)
  // Also need throttling for resize:  https://shiny.rstudio.com/articles/js-dashboard.html
  widget.onAfterShow = function(msg) { Shiny.bindAll(this); };
  widget.onResize = _.debounce( function(msg) { Shiny.bindAll(this); }, 150 );

  // Attach widget to panel (first widget is dock)
  dock = getDock(message.dockID);
  var ref = (message.refwidgetID !== null ? phosphorjs.find(dock.children(), (w) => {return w.id === message.refwidgetID}) : null);
  dock.addWidget(widget = widget, options = {mode: message.mode, ref: ref});

  // Tricky tricky tricky
  if (message.size !== null) {
    console.log("Setting size...");
    var layout = dock.saveLayout();
    setSize(layout.main, message.widgetID, message.size, (["split-top", "split-left"].includes(message.mode) ? 1 : -1));
    dock.restoreLayout(layout);
  }
});
