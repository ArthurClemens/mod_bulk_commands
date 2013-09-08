var action_bulk_commands = (function($) {

    var DELETE_ANIMATION_SPEED = 200,
        controlsTemplate,
        checkboxTemplate;
        
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
    '    </div>',
    '</div>'
    ].join("\n");
    
    checkboxTemplate = "<td><input type=\"checkbox\" class=\"adminBulkCommandCheck\" /></td>";
    
    var TableController = function($table) {
    
        var CHECKBOX_SELECTOR = ":checkbox.adminBulkCommandCheck",
            
            // vars
            $controls,
            
            // methods
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
        
        // Add Select  and commands button row
        fnAddControlsUI = function() {
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
                z_notify("bulk_delete",
                    {
                        data: JSON.stringify(checkedIds),
                        animationSpeed: DELETE_ANIMATION_SPEED,
                        tableId: $table.attr("id")
                    }
                );
            });
        };
        
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
                
        fnAddControlsUI();
        enhanceTable();
        updateUI();
    };
    
    $.widget("ui.adminLinkedTable", {
        _init: function() {
            new TableController($(this.element));
        }
    });
    
    return {
        onDeleteDone: function(o) {
            var $table = $("#" + o.tableId),
                animationSpeed = parseInt(o.animationSpeed, 10),
                delay = animationSpeed + 10;

            setTimeout(function() {
                // not yet defined
            }, delay);
        }
    }
    
}(jQuery));

