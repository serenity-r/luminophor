HTMLWidgets.widget({

  name: 'luminophor',

  type: 'output',

  factory: function(el, width, height) {

    var dock = null;
    var initLayout = null;
    var tmpLayout = null;

    return {

      renderValue: function(opts) {
        if (dock === null) {
  	      // Create DockPanel
  	      dock = new lumino.DockPanel();

  	      // Attach dock to el
  	      lumino.Widget.attach(dock, $('#'+el.id).find(".luminophor-shim")[0]);
        } else {
          this.removeWidgets(lumino.toArray(dock.widgets()));
        }

  	    // Add widgets
  	    opts.items.widgets.forEach(function(widget) {
  	      addWidget(
  	        dockID = el.id,
  	        widgetID = widget.widgetID,
  	        title = widget.title,
  	        caption = widget.caption,
  	        iconClass = widget.iconClass,
  	        closable = widget.closable,
  	        insertmode =  widget.insertmode,
  	        refwidgetID = widget.refwidgetID,
  	        relsize = widget.relsize,
  	        server = widget.server,
  	        ui = widget.ui
  	      );
  	    });

  	    // Save layout
  	    initLayout = dock.saveLayout();

        // When layout changes, track Flexdashboard compatibility of layout
        // send validFlexdash output to R
  	    mydock = getDock('lmobox');
  	    //mydock.onUpdateRequest = console.log("hello");
  	    mydock.onUpdateRequest = function(){Shiny.setInputValue(el.id + "_validFlexdash", validFlexdash(el.id))};

      },

      resize: function(width, height) {
        dock.update();
      },

      getDock: function() {
        return dock;
      },

      close: function() {
        dock.close();
        dock = null;
      },

      getLayout: function(init = false) {
        return (init ? initLayout : dock.saveLayout());
      },

      //html widget ^^

      restoreLayout: function() {
        dock.restoreLayout(initLayout);
      },

      maximizeWidget: function(widget) {
        tmpLayout = dock.saveLayout();
        dock.mode = 'single-document';
        dock.activateWidget(widget);
      },

      minimizeWidget: function(widget) {
        if (tmpLayout !== null) {
          dock.mode = 'multiple-document';
          dock.restoreLayout(tmpLayout);
          tmpLayout = null;
        }
      },

      removeWidgets: function(widgets) {
        widgets.forEach(widget => {
          Shiny.unbindAll(widget.node, true);
          // Replicating widget.close() from https://github.com/phosphorjs/phosphor/blob/8fee9108/packages/widgets/src/widget.ts#L586
          if (widget.parent) {
            widget.parent = null;
          } else if (widget.isAttached) {
            lumino.Widget.detach(widget);
          }
        });
      }
    };
  }
});

// Helper function to get an existing luminophor object via the htmlWidgets object.
function getDock(id) {

  // Get the HTMLWidgets object
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  var luminophorObj = null;
  if( typeof(htmlWidgetsObj) !== "undefined"){
    // Use the getDock method we created to get the underlying gridstack
    luminophorObj = htmlWidgetsObj.getDock();
  }

  return(luminophorObj);
}

// This currently assumes that index of widget is related to top/left and bottom/right
function setSize(layout, widgetID, size, dir) {
  // Should always have a child at this stage
  if (layout.children !== undefined) {
    var ind = layout.children.findIndex((child) => {
      if (child.widgets !== undefined) {
        return child.widgets.some((w) => {return w.id === widgetID});
      } else if (child.children !== undefined) {
        return setSize(child, widgetID, size, dir);
      } else {
        return false;
      }
    });

    if (ind !== -1) {
      var total = layout.sizes[ind] + layout.sizes[ind+dir];
      layout.sizes[ind] = total*size;
      layout.sizes[ind+dir] = total*(1-size);
    }
  }
}

function maximizeWidget(dockID, widgetID) {
  var dock = getDock(dockID);
  var widget = lumino.find(dock.widgets(), (w) => {return w.id === widgetID});

  HTMLWidgets.find('#'+dockID).maximizeWidget(widget);
}

function minimizeWidget(dockID, widgetID) {
  var dock = getDock(dockID);
  var widget = lumino.find(dock.widgets(), (w) => {return w.id === widgetID});

  HTMLWidgets.find('#'+dockID).minimizeWidget(widget);
}

function removeWidgets(dockID, widgetIDs, all) {
  var dock = getDock(dockID);
  var widgets = lumino.toArray(dock.widgets());
  if (!all) {
    widgets = lumino.toArray(lumino.filter(widgets, widget => { return(widgetIDs.includes(widget.id)) }));
  }
  HTMLWidgets.find('#'+dockID).removeWidgets(widgets);
}

function addWidget(dockID, widgetID, title = "Widget", caption = "Widget", iconClass = "", closable = true, insertmode = "tab-after", refwidgetID = null, relsize = null, server = false, ui = null) {
  // Add widget content to DOM
  $('#'+dockID).append('<div id="' + widgetID + '" class="widget-content' + (server ? ' shiny-html-output' : '') + '"></div>');

  if (ui !== null) {
    Shiny.renderContent(document.getElementById(widgetID), ui, "beforeEnd");
  }

  // Create widget and bind content
  var widget = new lumino.Widget({node: document.getElementById(widgetID)});

  // Add title and make closable
  widget.title.label = title;
  widget.title.caption = caption;
  widget.title.iconClass = iconClass;
  widget.title.closable = closable;

  // Need to rebind Shiny on certain events (for now, show and resize only)
  // Also need throttling for resize:  https://shiny.rstudio.com/articles/js-dashboard.html
  widget.onCloseRequest = function(msg) { HTMLWidgets.find("#" + $(this.parent.node).closest(".luminophor").attr("id")).removeWidgets(Array(this)); // Need to use the HTMLWidget method or it won't work
  };
  widget.onResize = _.debounce( function(msg) { Shiny.bindAll(this); }, 150 );

  // Attach widget to panel (first widget is dock)
  dock = getDock(dockID);
  var ref = (refwidgetID !== null ? lumino.find(dock.children(), (w) => {return w.id === refwidgetID}) : null);
  dock.addWidget(widget = widget, options = {mode: insertmode, ref: ref});

  // Tricky tricky tricky
  if (relsize !== null) {
    var layout = dock.saveLayout();
    setSize(layout.main, widgetID, relsize, (["split-top", "split-left"].includes(insertmode) ? 1 : -1));
    dock.restoreLayout(layout);
  }
}

// Send the layout to R
// JSON.stringify to deal with circular references: https://stackoverflow.com/questions/11616630/how-can-i-print-a-circular-structure-in-a-json-like-format
function getLayout(dockID) {
  var dock = getDock(dockID);
  var myLayout = dock.saveLayout();
  var cache = [];
  Shiny.setInputValue(dockID + "_layout:layout_handler", JSON.stringify(myLayout.main, function(key, value) {
    if (typeof value === 'object' && value !== null) {
        if (cache.indexOf(value) !== -1) {
            // Duplicate reference found, discard key
            return;
        }
        // Store value in our collection
        cache.push(value);
    }
    return value;
}));
}

// Check if layout is Flexdashboard compatible
function validFlexdash(dockID) {
  if (dockID === null) {
    return false;
  }
  var dock = getDock(dockID);
  var myLayout = dock.saveLayout();

  //if null, valid layout
  if (myLayout.main === null) {
    return true}

  //if there is only one box (no children), valid layout
  if (myLayout.main !== null && myLayout.main.children === undefined) {
    return true}

  // if there are any children, check for grandchildren
  if (myLayout.main !== null && myLayout.main.children !== undefined) {
    var arya = myLayout.main.children.map(function(children) {

    // if there are no grandchildren, valid layout
    if (children.children === undefined) {return true}

    //if there are grandchildren, check for great-grandchildren
    if (children.children !== undefined) {
      var stark = children.children.map(function(children) {
        // if no great-grandchildren, valid layout
        if (children.children === undefined) {return true}
        //if great-grandchildren, NOT valid layout
        if (children.children !== undefined) {return false}
          });
    //return true/false results for great-grandchildren
    return stark.reduce(function(accumulator, currentValue, currentIndex, array) {
      return accumulator && currentValue;
          });
        }
      });
    //then return true/false results for grandchildren
    return arya.reduce(function(accumulator, currentValue, currentIndex, array) {
      return accumulator && currentValue;
          });
      }

   return true;
}

//function isvalidFlexdash(dockID){
  //Shiny.setInputValue(dockID + "_validFlexdash", validFlexdash())};


// ---- R -> Javascript

// Note:  Might want to make widget ids boxID + widgetID so can have same widgetID in different stacks.  Right now, based on best practices, items must have unique IDs, even across different boxes

//listeners!!!!!!

// Custom handler to add a new widget
Shiny.addCustomMessageHandler('luminophor:addWidget', function(message) {
  addWidget(
    dockID = message.dockID,
    widgetID = message.widgetID,
    title = message.title,
    caption = message.caption,
    iconClass = message.iconClass,
    closable = message.closable,
    insertmode =  message.insertmode,
    refwidgetID = message.refwidgetID,
    relsize = message.relsize,
    server = message.server);
});

// Custom handler to maximize a widget
Shiny.addCustomMessageHandler('luminophor:maximizeWidget', function(message) {
  maximizeWidget(
    dockID = message.dockID,
    widgetID = message.widgetID
  );
});

// Custom handler to minimize a widget
Shiny.addCustomMessageHandler('luminophor:minimizeWidget', function(message) {
  minimizeWidget(
    dockID = message.dockID,
    widgetID = message.widgetID
  );
});

// Custom handler to remove a widget
Shiny.addCustomMessageHandler('luminophor:removeWidgets', function(message) {
  removeWidgets(
    dockID = message.dockID,
    widgetIDs = message.widgetIDs,
    all = message.all
  );
});

// Custom handler to get layout
Shiny.addCustomMessageHandler('luminophor:getLayout', function(message) {
  getLayout(
    dockID = message.dockID
  );
});

// Custom handler to get layout compatibility
Shiny.addCustomMessageHandler('luminophor:validFlexdash', function(message) {
  validFlexdash(
    dockID = message.dockID
  );
});
