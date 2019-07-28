HTMLWidgets.widget({

  name: 'phosphorr',

  type: 'output',

  factory: function(el, width, height) {

    var dock = null;
    var initLayout = null;
    var tmpLayout = null;

    return {

      renderValue: function(opts) {
        if (dock === null) {
  	      // Create DockPanel
  	      dock = new phosphorjs.DockPanel();

  	      // Attach dock to el
  	      phosphorjs.Widget.attach(dock, $('#'+el.id).find(".phosphorr-shim")[0]);

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
        }
      },

      resize: function(width, height) {
        dock.update();
      },

      getDock: function() {
        return dock;
      },

      getLayout: function(init = false) {
        return (init ? initLayout : dock.saveLayout());
      },

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
    // Use the getDock method we created to get the underlying gridstack
    phosphorrObj = htmlWidgetsObj.getDock();
  }

  return(phosphorrObj);
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
  var widget = phosphorjs.find(dock.widgets(), (w) => {return w.id === widgetID});

  HTMLWidgets.find("#" + dockID).maximizeWidget(widget);
}

function minimizeWidget(dockID, widgetID) {
  var dock = getDock(dockID);
  var widget = phosphorjs.find(dock.widgets(), (w) => {return w.id === widgetID});

  HTMLWidgets.find("#" + dockID).minimizeWidget(widget);
}

function addWidget(dockID, widgetID, title = "Widget", caption = "Widget", iconClass = "", closable = true, insertmode = "tab-after", refwidgetID = null, relsize = null, server = false, ui = null) {
  Shiny.unbindAll($('#'+dockID)[0]);

  // Add widget content to DOM
  $('#'+dockID).append('<div id="' + widgetID + '" class="widget-content' + (server ? ' shiny-html-output' : '') + '">' + (ui !== null ? ui : '') + '</div>');

  // Create widget and bind content
  var widget = new phosphorjs.Widget({node: document.getElementById(widgetID)});

  // Add title and make closable
  widget.title.label = title;
  widget.title.caption = caption;
  widget.title.iconClass = iconClass;
  widget.title.closable = closable;

  // Need to rebind Shiny on certain events (for now, show and resize only)
  // Also need throttling for resize:  https://shiny.rstudio.com/articles/js-dashboard.html
  widget.onAfterShow = function(msg) { Shiny.bindAll(this); };
  widget.onResize = _.debounce( function(msg) { Shiny.bindAll(this); }, 150 );

  // Attach widget to panel (first widget is dock)
  dock = getDock(dockID);
  var ref = (refwidgetID !== null ? phosphorjs.find(dock.children(), (w) => {return w.id === refwidgetID}) : null);
  dock.addWidget(widget = widget, options = {mode: insertmode, ref: ref});

  // Tricky tricky tricky
  if (relsize !== null) {
    var layout = dock.saveLayout();
    setSize(layout.main, widgetID, relsize, (["split-top", "split-left"].includes(insertmode) ? 1 : -1));
    dock.restoreLayout(layout);
  }

  Shiny.bindAll($('#'+dockID)[0]);
}

// ---- R -> Javascript

// Note:  Might want to make widget ids boxID + widgetID so can have same widgetID in different stacks.  Right now, based on best practices, items must have unique IDs, even across different boxes

// Custom handler to add a new widget
Shiny.addCustomMessageHandler('phosphorr:addWidget', function(message) {
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
Shiny.addCustomMessageHandler('phosphorr:maximizeWidget', function(message) {
  maximizeWidget(
    dockID = message.dockID,
    widgetID = message.widgetID
  );
});

// Custom handler to minimize a widget
Shiny.addCustomMessageHandler('phosphorr:minimizeWidget', function(message) {
  minimizeWidget(
    dockID = message.dockID,
    widgetID = message.widgetID
  );
});
