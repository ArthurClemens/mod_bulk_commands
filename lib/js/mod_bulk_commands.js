var action_bulk_commands = (function($) {

    var ANIMATION_SPEED = 200,
        controlsTemplate,
        $table;
        
    controlsTemplate = [
    '<div class="btn-toolbar adminBulkCommands">',
    '    <div class="btn-group dropdown">',
    '        <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">Select <b class="caret"></b></a>',
    '        <ul class="dropdown-menu" role="menu">',
    '            <li><a class="checkAllBtn" href="#">All</a></li>',
    '            <li><a class="uncheckAllBtn" href="#">None</a></li>',
    '        </ul>',
    '    </div>',
    '    <div class="btn-group adminCommand">',
    '        <a class="btn deleteBtn" href="#">Delete</a>',
    '    </div>',
    '</div>'
    ].join("\n");
    
    $.widget("ui.adminLinkedTable",
    {
        _init: function() {
            var fnEnhanceTable,
                fnUpdateUI,
                fnUpdateRowColors,
                fnUpdateButtonStates;

            $table = $(this.element);
                            
            fnAddControlsUI = function() {
                var $controls = $(controlsTemplate).insertBefore($table);

                // Check all button
                $(".checkAllBtn", $controls).on("click", function (e) {
                    e.preventDefault();
                    $table.find(":checkbox").prop("checked", true);
                    fnUpdateUI();
                });

                // Uncheck all button
                $(".uncheckAllBtn", $controls).on("click", function (e) {
                    e.preventDefault();
                    $table.find(":checkbox").prop("checked", false);
                    fnUpdateUI();
                });
                
                // Delete button
                $(".deleteBtn", $controls).on("click", function() {
                    var checkedIds = [];
                    $table.find(":checkbox:checked").each(function() {
                        var $tr = $(this).parents("tr"),
                            href = $tr.attr("data-href"),
                            rowId = $tr.attr("id")
                        checkedIds.push({
                            href: href,
                            rowId: rowId
                        });
                    });
                    z_notify("bulk_delete", {data: JSON.stringify(checkedIds)}, {animationSpeed: ANIMATION_SPEED});
                });
            };

            fnEnhanceTable = function() {
                $("tr:first", $table).prepend("<th></th");
                $("tr:not(:first)", $table).each(function() {
                    var $tr = $(this);
                    $tr.find("td:first").each(function() {
                        if (!$(this).attr("colspan")) {
                            $(this).parent("tr").prepend("<td><input type=\"checkbox\" /></td>");
                        }
                    });
                });
                
                // Each checkbox
                $("input[type='checkbox']", $table).on("click", function(e) {
                    fnUpdateUI();
                });
            };

            fnUpdateUI = function() {
                fnUpdateRowColors();
                fnUpdateButtonStates();
            };
            
            fnUpdateRowColors = function() {                
                var count = $table.find(":checkbox:checked").length,
                    MARKER_CLASS_NAME = "warning";

                if (count > 0) {
                    $table.removeClass("table-striped");
                } else {
                    $table.addClass("table-striped");
                }
                $table.find(":checkbox:checked")
                  .parents("tr")
                  .addClass(MARKER_CLASS_NAME);
                $table.find(":checkbox:not(':checked')")
                  .parents("tr")
                  .removeClass(MARKER_CLASS_NAME);
            };
            
            fnUpdateButtonStates = function() {
                var checked,
                    total;

                total = $table.find(":checkbox").length;
                if (total === 0) {
                    $(".adminBulkCommands .dropdown-toggle").addClass("disabled");
                } else {
                    $(".adminBulkCommands .dropdown-toggle").removeClass("disabled");
                }

                checked = $table.find(":checkbox:checked");
                if (checked.length !== 0) {
                    $(".adminCommand").show();
                } else {
                    $(".adminCommand").hide();
                }
            };

            fnAddControlsUI();
            fnEnhanceTable();
            fnUpdateUI();
        }
    });
    
    return {
        onDeleteDone: function() {
            setTimeout(function() {
                var rowCount = $("tr", $table).length;
                if (rowCount === 1) {
                    // reload page 1
                    var $first = $(".pagination").find("li:first");
                    if ($first.length > 0) {
                        location.href = $first.find("a").attr("href");
                    }
                }
            }, ANIMATION_SPEED + 10);
        }
    }
    
}(jQuery));

