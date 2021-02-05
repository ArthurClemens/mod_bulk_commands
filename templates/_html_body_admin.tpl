
{% with m.config.mod_bulk_commands.pages.value|split:",",
        m.config.mod_bulk_commands.commands.value
   as
        pages,
        commands
%}
        <script type="text/javascript">
            var action_bulk_commands_attrs = "{{commands}}";
        </script>
        {% lib
            "js/mod_bulk_commands.js"
            "css/mod_bulk_commands.css"
        %}
{% endwith %}
