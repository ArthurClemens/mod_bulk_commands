(function($) {
    var uid = 0;
    $.getUID = function() {
        uid++;
        return 'jQ-uid-'+uid;
    };
    $.fn.getUID = function() {
        if(!this.length) {
            return 0;
        }
        var fst = this.first(), id = fst.attr('id');
        if(!id) {
            id = $.getUID();
            fst.attr('id', id);
        }
        return id;
    }
})(jQuery);

var action_bulk_commands_attrs;

var action_bulk_commands = (function($) {

    var ANIMATION_SPEED = 200,
        controllers = {};
    
    var controlsTmpl = [
    '<div class="widget-content adminBulkCommands">',
    '    <div class="btn-toolbar">',
    '        <div class="btn-group dropdown">',
    '            <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">Select <b class="caret"></b></a>',
    '            <ul class="dropdown-menu" role="menu">',
    '                <li><a data-action="check-all" href="#">All</a></li>',
    '                <li><a data-action="uncheck-all" href="#">None</a></li>',
    '            </ul>',
    '        </div>',
    '        PLACEHOLDER_DELETE',
    '        PLACEHOLDER_ATTRIBUTES_DROPDOWN',
    '    </div>',
    '</div>'
    ].join("\n");
    
    var controlsTmplDelete = [
    '        <div class="btn-group adminBulkCommand">',
    '            <a class="btn adminBulkCommandsDelete" href="#">Delete</a>',
    '        </div>'
    ].join("\n");
    
    var controlsTmplAttributes = [
    '        <div class="btn-group dropdown adminBulkCommand">',
    '            <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">Set state... <b class="caret"></b></a>',
    '            <ul class="dropdown-menu" role="menu">',
    '                PLACEHOLDER_ATTRIBUTES',
    '            </ul>',
    '        </div>',
    ].join("\n");
    
    var controlsTmplAttributesSeparator = "\n" + 
    '                <li class="divider"></li>\n';
    
    var controlsTmplAttributesPublished = [
    '                <li><a class="adminBulkCommandsHeader" href="#">Published</a></li>',
    '                <li><a data-action="published-on" href="#">on</a></li>',
    '                <li><a data-action="published-off" href="#">off</a></li>',
    ].join("\n");
    
    var controlsTmplAttributesFeatured = [
    '                <li><a class="adminBulkCommandsHeader" href="#">Featured</a></li>',
    '                <li><a data-action="featured-on" href="#">on</a></li>',
    '                <li><a data-action="featured-off" href="#">off</a></li>',
    ].join("\n");
    
    var controlsTmplAttributesProtected = [
    '                <li><a class="adminBulkCommandsHeader" href="#">Protected</a></li>',
    '                <li><a data-action="protected-on" href="#">on</a></li>',
    '                <li><a data-action="protected-off" href="#">off</a></li>',
    ].join("\n");
    
    var deletePopoverControlsTmpl = [
        '<a href="#" class="btn btn-danger" data-action="delete">Delete</a>',
        '<a href="#" class="btn" data-action="cancel">Cancel</a>'
        ].join("\n");
    
    var checkboxTmpl = "<td><input type=\"checkbox\" class=\"adminBulkCommandCheck\" /></td>";
    
    var createControlsTemplate = function() {
        var controlsTemplate = controlsTmpl,
            attributes = "",
            attributeList = [],
            attributeListElems = "",
            commands;
            
        if (!action_bulk_commands_attrs) {
            action_bulk_commands_attrs = "delete,published,featured,protected";
        }
        commands = action_bulk_commands_attrs.split(/\s*,\s*/);
        if (commands.indexOf("published") !== -1 || 
            commands.indexOf("featured") !== -1 ||
            commands.indexOf("protected") !== -1) {

            if (commands.indexOf("published") !== -1) {
                attributeList.push(controlsTmplAttributesPublished);   
            }
            if (commands.indexOf("featured") !== -1) {
                attributeList.push(controlsTmplAttributesFeatured);   
            }
            if (commands.indexOf("protected") !== -1) {
                attributeList.push(controlsTmplAttributesProtected);   
            }
            attributeListElems = attributeList
              .join(controlsTmplAttributesSeparator);
            attributes = controlsTmplAttributes
              .replace('PLACEHOLDER_ATTRIBUTES', attributeListElems);
        }
        controlsTemplate = controlsTemplate
          .replace('PLACEHOLDER_ATTRIBUTES_DROPDOWN', attributes);
        
        if (commands.indexOf("delete") === -1) {
            controlsTmplDelete = "";
        }
        controlsTemplate = controlsTemplate
          .replace('PLACEHOLDER_DELETE', controlsTmplDelete);
        
        return controlsTemplate;
    }
    
    var tableController = function($table) {
    
        var CHECKBOX_SELECTOR = ":checkbox.adminBulkCommandCheck",
            
            // vars
            $controls,
            
            // methods
            setUpControlsUI,
            getCheckedIds,
            sendCommand,
            checkTableChangeable,
            enhanceTable,
            updateUI,
            updateButtonStates,
            totalCount,
            checkedCount,
            checkboxesChecked
            ;

        // Make sure this table has a unique ID
        $table.getUID();
        
        // Add our class
        $table.addClass("adminBulkCommandsList");
        
        // Add Select  and commands button row
        setUpControlsUI = function() {
            
            var controlsTemplate = createControlsTemplate();
            
            $controls = $(controlsTemplate).insertBefore($table).hide();
            
            $(".adminBulkCommandsDelete", $controls).popover({
                placement: "top",
                html: true,
                content: deletePopoverControlsTmpl
            }).on("click", function (e) {
                e.preventDefault();
                e.stopPropagation();
            });
            
            // Check all button
            $("a[data-action=check-all]", $controls).on("click", function (e) {
                e.preventDefault();
                e.stopPropagation();
                $table.find(CHECKBOX_SELECTOR).prop("checked", true);
                updateUI();
                $(this)
                  .closest(".dropdown")
                  .find(".dropdown-toggle")
                  .trigger("click");
            });

            // Uncheck all button
            $("a[data-action=uncheck-all]", $controls).on("click", function (e) {
                e.preventDefault();
                e.stopPropagation();
                $table.find(CHECKBOX_SELECTOR).prop("checked", false);
                updateUI();
                $(this)
                  .closest(".dropdown")
                  .find(".dropdown-toggle")
                  .trigger("click");
            });
            
            // Delete button
            $controls.on("click", "a[data-action=delete]", function(e) {
                e.preventDefault();
                e.stopPropagation();
                sendCommand("bulk_delete");
                $(".adminBulkCommandsDelete", $controls).popover("hide");
            });
            $controls.on("click", "a[data-action=cancel]", function(e) {
                e.preventDefault();
                e.stopPropagation();
                $(".adminBulkCommandsDelete", $controls).popover("hide");
            });
            
            // Published
            $("a[data-action=published-on]", $controls).on("click", function(e) {
                e.preventDefault();
                e.stopPropagation();
                sendCommand("published_true");
            });
            $("a[data-action=published-off]", $controls).on("click", function(e) {
                e.preventDefault();
                e.stopPropagation();
                sendCommand("published_false");
            });
            
            // Featured
            $("a[data-action=featured-on]", $controls).on("click", function(e) {
                e.preventDefault();
                e.stopPropagation();
                sendCommand("featured_true");
            });
            $("a[data-action=featured-off]", $controls).on("click", function(e) {
                e.preventDefault();
                e.stopPropagation();
                sendCommand("featured_false");
            });
            
            // Protected
            $("a[data-action=protected-on]", $controls).on("click", function(e) {
                e.preventDefault();
                e.stopPropagation();
                sendCommand("protected_true");
            });
            $("a[data-action=protected-off]", $controls).on("click", function(e) {
                e.preventDefault();
                e.stopPropagation();
                sendCommand("protected_false");
            });
        };
        
        getCheckedIds = function() {
            var checkedIds = [];
            checkboxesChecked().each(function() {
                var $tr = $(this).parents("tr"),
                    href = $tr.attr("data-href"),
                    rowId = $tr.attr("id")
                checkedIds.push({
                    href: href,
                    rowId: rowId
                });
            });
            return checkedIds;
        }
        
        sendCommand = function(command) {
            z_notify(command,
                {
                    data: JSON.stringify(getCheckedIds()),
                    tableId: $table.attr("id")
                }
            );
        }
        
        // Check if table has any non-colspan rows
        checkTableChangeable = function() {
            var changeable = false;
            $("tr:not(:first)", $table).each(function() {
                var $tr = $(this);
                $tr.find("td:first").each(function() {
                    var $td = $(this);
                    if (!$td.attr("colspan")) {
                        changeable = true;
                    }
                });
            });
            return changeable;
        };
        
        // Add an extra column with a checkbox
        // except for rows that have a colspan
        enhanceTable = function() {
            if (!checkTableChangeable()) {
                return;
            }
            
            // Make sure that all rows have a unique ID
            $table.find("tr").getUID();
            
            $("tr:first", $table).prepend("<th></th");
            $("tr:not(:first)", $table).each(function() {
                $(this).prepend(checkboxTmpl);
            });
            
            // Each checkbox
            $(CHECKBOX_SELECTOR, $table).on("click", function(e) {
                updateUI();
            });
        };
        
        updateUI = function() {
            updateButtonStates();
        };
        
        updateButtonStates = function() {
            var FADE_IN_DURATION = 150;
            if (totalCount() === 0) {
                $controls.fadeOut(FADE_IN_DURATION);
            } else {
                $controls.fadeIn(FADE_IN_DURATION);
            }
            if (checkedCount() !== 0) {
                $(".adminBulkCommand", $controls).fadeIn(FADE_IN_DURATION);
            } else {
                $(".adminBulkCommand", $controls).fadeOut(FADE_IN_DURATION);
            }
        };
        
        checkboxesChecked = function() {
            return $table.find(CHECKBOX_SELECTOR + ":checked");
        };
        
        checkedCount = function() {
            return checkboxesChecked().length;
        };
        
        totalCount = function() {
            return $table.find(CHECKBOX_SELECTOR).length;
        };
                
        setUpControlsUI();
        enhanceTable();
        updateUI();
        
        return {
            update: function() {
                updateUI();
            }
        }
    };
    
    // Monkey patch the widget's init function
    $.ui.adminLinkedTable.prototype._originalInit = $.ui.adminLinkedTable.prototype._init;
    $.widget("ui.adminLinkedTable", $.extend({}, $.ui.adminLinkedTable.prototype, {
        _init: function() { 
            var $table,
                controller,
                initResult;
            initResult = $.ui.adminLinkedTable.prototype._originalInit.apply(this, arguments);
            
            // disable bulk commands on admin/comments page
            if (/admin\/comments/.test(document.location)) {
                return initResult;
            }
            
            // disable bulk commands on admin/config page
            if (/admin\/config/.test(document.location)) {
                return initResult;
            }
            
            $table = $(this.element);
            controller = new tableController($table);
            controllers[$table] = controller;
            return initResult;
        }
    }));
    
    return {
        onDone: function(o) {
            var $table = $("#" + o.tableId),
                animationSpeed = ANIMATION_SPEED,
                delay = animationSpeed + 10;
                
            setTimeout(function() {
                var controller = controllers[$table];
                controller.update();
                
                // with a delete go to the first page of the results
                // to prevent an empty last page with no exit
                if (o.command === "delete") {
                    var $pagination = $(".pagination");

                    if ($pagination.length > 0) {
                        // find the first link that does not start with #
                        var validHrefs = $pagination
                                            .find("li a")
                                            .filter(function() {
                            var href = $(this).attr("href");
                            return (href.match(/^[^#].*$/) != null);
                        });
                        if (validHrefs.length > 0) {
                            document.location = validHrefs[0];
                            location.reload();
                        }
                    }
                }
            }, delay);
        },
        
        // used to update a row element when it is not removed
        highlight: function(o) {
            var $row = $("#" + o.rowId),
                is_published = o.is_published;
            
            if (is_published !== undefined) {
                if (is_published) {
                    $row.removeClass("unpublished");
                } else {
                    $row.addClass("unpublished");
                }
            }
            $row.action_bulk_commands_highlight();
        }
    }
    
}(jQuery));


jQuery.fn.action_bulk_commands_highlight = function() {
   $(this).each(function () {
        var el = $(this);
        $("<div/>")
        .width(el.outerWidth())
        .height(el.outerHeight())
        .css({
            "position": "absolute",
            "left": el.offset().left,
            "top": el.offset().top,
            "background-color": "#ff9",
            "opacity": .6,
            "z-index": 1
        })
        .appendTo("body")
        .fadeOut(1000)
        .queue(function () {
            $(this).remove();
        });
    });
}