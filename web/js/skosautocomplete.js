/*
 * Widget to configure an autocompletion widget with suggestions from a skos vocabulary
 */

YUI.add('skosautocomplete', function(Y) {
    function SkosAutoComplete(config) {
	SkosAutoComplete.superclass.constructor.apply(this, arguments);
    }
    SkosAutoComplete.NAME = "aclist"; // use same name as Y.Plugin.AutoComplete to inherit css
    SkosAutoComplete.NS = "skosautocomplete";
    SkosAutoComplete.ATTRS = {
	handler: { value: null },  // on select we call handler ... 
	caller:  { value: null },  //  with caller and 
	context: { value: {}   },  //  context as parameters
    };

    Y.extend(SkosAutoComplete, Y.Plugin.AutoComplete, {
	
	initializer: function(args) {
	    var parentNode = this.DEF_PARENT_NODE;

	    // handler to call when item selected from autocompletion suggestions:
	    var caller  = this.get('caller');
	    var handler = this.get('handler');
	    var context = this.get('context');
	    this.on("select", handler, caller, context);
		
	    // infoNode is the overlay with tooltips when hovering over suggested terms
	    this.infoNode = new Y.Overlay({}).render(parentNode);
	    this.on("activeItemChange",  this.onSuggestionHover, this);
	    this.on("hoveredItemChange", this.onSuggestionHover, this);
	},
	
	onSuggestionHover : function(ev) {
	    var infoNode = this.infoNode,
	    active = ev.newVal,
	    body = '';
	    if(active && active.getData().result.raw.info) {
		var scope = active.getData().result.raw.info.scopeNotes[0];
		var defin = active.getData().result.raw.info.definitions[0];
		var alts =  active.getData().result.raw.info.altLabels;
		var img =  active.getData().result.raw.info.images[0];
		if (scope && scope.en) { body += "<div class='scope'>"+scope.en+"</div>"; }
		if (defin && defin.en) { body += "<div class='defin'>"+defin.en+"</div>"; }
		if (img) { body += "<img class='depiction' src='"+img+"'>"; }
		for (var i=0; i<alts.length; i++) {
		    body += "<span class='altLabel'>" +alts[i] + "</span>" ;
		}
	    }
	    if(body) {
		var ratio = ev.newVal.getX()/window.innerWidth;
		if (ratio < 0.4) { // make suggestions appear on the left or the right:
		    infoNode.set("align", {node:active, points:[Y.WidgetPositionAlign.TL, Y.WidgetPositionAlign.TR]});
		} else {
		    infoNode.set("align", {node:active, points:[Y.WidgetPositionAlign.TR, Y.WidgetPositionAlign.TL]});
		}
		infoNode.set("bodyContent", body);
		infoNode.show();
	    } else {
		infoNode.hide();
	    }
	},
    });

    Y.Plugin.SkosAutoComplete = SkosAutoComplete;

}, '0.0.1', { requires: [
    'node','event','event-custom', 'autocomplete','overlay','recordset','io-base','json','querystring-stringify-simple'
]
	    });
