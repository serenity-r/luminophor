HTMLWidgets.widget({

  name: 'phosphorr',

  type: 'output',

  factory: function(el, width, height) {

    var box = null;

    return {

      renderValue: function(x) {

        if (box === null) {
          // Create BoxPanel
  	      box = new phosphorjs.BoxPanel({ direction: 'left-to-right', spacing: 0 });

  	      // Create DockPanel
  	      var dock = new phosphorjs.DockPanel();

  	      // Create ContentWidget (right now, uses predefined function)
  	      var r1 = new phosphorjs.ContentWidget('Red');
  	      var r2 = new phosphorjs.ContentWidget('Blue');

  	      // Add widgets in appropriate places - this can be done in any order
  	      dock.addWidget(r1);
  	      dock.addWidget(r2);
  	      box.addWidget(dock);

  	      // Attach BoxPanel to el
  	      phosphorjs.Widget.attach(box, el);
        }
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
