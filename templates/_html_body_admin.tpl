{% with m.config.mod_bulk_commands.pages.value|split:"," as pages %}
    {% if selected|member:pages %}
        {% lib
            "js/mod_bulk_commands.js"
            "css/mod_bulk_commands.css"
        %}
    {% endif %}
{% endwith %}