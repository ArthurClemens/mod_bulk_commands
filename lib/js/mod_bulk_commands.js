var action_bulk_commands = (function($) {

    var DELETE_ANIMATION_SPEED = 200,
        controlsTemplate,
        checkboxTemplate,
        controllers = {};
        
    controlsTemplate = [
    '<div class="widget-content adminBulkCommands">',
    '    <div class="btn-toolbar">',
    '        <div class="btn-group dropdown">',
    '            <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">Select <b class="caret"></b></a>',
    '            <ul class="dropdown-menu" role="menu">',
    '                <li><a class="checkAllBtn" href="#">All</a></li>',
    '                <li><a class="uncheckAllBtn" href="#">None</a></li>',
    '            </ul>',
    '        </div>',
    '        <div class="btn-group adminBulkCommand">',
    '            <a class="btn deleteBtn" href="#">Delete</a>',
    '        </div>',
    '        <div class="btn-group dropdown adminBulkCommand">',
    '            <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">Set state... <b class="caret"></b></a>',
    '            <ul class="dropdown-menu" role="menu">',
    '                <li><a class="adminBulkCommandsHeader" href="#">Published</a></li>',
    '                <li><a class="publishedOnBtn" href="#">on</a></li>',
    '                <li><a class="publishedOffBtn" href="#">off</a></li>',
    '                <li class="divider"></li>',
    '                <li><a class="adminBulkCommandsHeader" href="#">Featured</a></li>',
    '                <li><a class="featuredOnBtn" href="#">on</a></li>',
    '                <li><a class="featuredOffBtn" href="#">off</a></li>',
    '                <li class="divider"></li>',
    '                <li><a class="adminBulkCommandsHeader" href="#">Protected</a></li>',
    '                <li><a class="protectedOnBtn" href="#">on</a></li>',
    '                <li><a class="protectedOffBtn" href="#">off</a></li>',
    '            </ul>',
    '        </div>',
    '    </div>',
    '</div>'
    ].join("\n");
    
    checkboxTemplate = "<td><input type=\"checkbox\" class=\"adminBulkCommandCheck\" /></td>";
    
    var tableController = function($table) {
    
        var CHECKBOX_SELECTOR = ":checkbox.adminBulkCommandCheck",
            
            // vars
            $controls,
            
            // methods
            controlsUI,
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
        $table.uniqueId();
        
        // Add our class
        $table.addClass("adminBulkCommandsList");
        
        // Add Select  and commands button row
        controlsUI = function() {
            $controls = $(controlsTemplate).insertBefore($table);
            
            // Check all button
            $(".checkAllBtn", $controls).on("click", function (e) {
                e.preventDefault();
                $table.find(CHECKBOX_SELECTOR).prop("checked", true);
                updateUI();
            });

            // Uncheck all button
            $(".uncheckAllBtn", $controls).on("click", function (e) {
                e.preventDefault();
                $table.find(CHECKBOX_SELECTOR).prop("checked", false);
                updateUI();
            });
            
            // Delete button
            $(".deleteBtn", $controls).on("click", function() {
                sendCommand("bulk_delete");
            });
            
            // Published
            $(".publishedOnBtn", $controls).on("click", function() {
                sendCommand("published_true");
            });
            $(".publishedOffBtn", $controls).on("click", function() {
                sendCommand("published_false");
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
                    animationSpeed: DELETE_ANIMATION_SPEED,
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
            $table.find("tr").uniqueId();
            
            $("tr:first", $table).prepend("<th></th");
            $("tr:not(:first)", $table).each(function() {
                $(this).prepend(checkboxTemplate);
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
            if (totalCount() === 0) {
                $controls.hide();
            } else {
                $controls.show();
            }
            if (checkedCount() !== 0) {
                $(".adminBulkCommand", $controls).show();
            } else {
                $(".adminBulkCommand", $controls).hide();
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
                
        controlsUI();
        enhanceTable();
        updateUI();
        
        return {
            update: function() {
                updateUI();
            }
        }
    };
    
    $.widget("ui.adminLinkedTable", {
        _init: function() {
            var $table = $(this.element),
                controller = new tableController($table);
            controllers[$table] = controller;
        }
    });
    
    return {
        onDone: function(o) {
            var $table = $("#" + o.tableId),
                animationSpeed = parseInt(o.animationSpeed, 10),
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
                animationSpeed = parseInt(o.animationSpeed, 10),
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
            "z-index": 9999999
        })
        .appendTo("body")
        .fadeOut(1000)
        .queue(function () {
            $(this).remove();
        });
    });
}