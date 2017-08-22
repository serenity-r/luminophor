HTMLWidgets.widget({

  name: 'phosphorus',

  type: 'output',

  factory: function(el, width, height) {

    var main = null;

    return {

      renderValue: function(x) {

        if (main === null) {
          // Create BoxPanel
  	      main = new phosphorjs.BoxPanel({ direction: 'left-to-right', spacing: 0 });
  	      main.id = 'main';

  	      // Create DockPanel
  	      var dock = new phosphorjs.DockPanel();
  	      dock.id = 'dock';

  	      // Create ContentWidget (right now, uses predefined function)
  	      var r1 = new phosphorjs.ContentWidget('Red');

  	      // Add widgets in appropriate places - this can be done in any order
  	      dock.addWidget(r1);
  	      main.addWidget(dock);

  	      // Attach BoxPanel to document.body (could be any element)
  	      phosphorjs.Widget.attach(main, el);
        }
      },

      resize: function(width, height) {
        main.update();
      }

    };
  }
});
