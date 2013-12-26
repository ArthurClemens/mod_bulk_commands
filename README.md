# Bulk actions for Zotonic admin

Select one or more items from the list, then perform an action. Implemented commands:

* Delete
* Set state: published, featured, protected


## Checks

* ACL rights are maintained.
* It is not possible to delete a page that is protected.


## Installation

Zotonic `>= 0.7`:

        zotonic installmodule mod_bulk_commands

Zotonic `<= 0.6`:

        git clone https://github.com/ArthurClemens/mod_bulk_commands.git mod_bulk_commands

Activate the module in Admin > System > Modules.


## Configuration

At activation, a config value `module_bulk_commands.pages` is set. This contains a list of comma-separated pages where the module should appear.

Default value: `dashboard,overview,media`.


## Requirements

JavaScript should be enabled in the browser.


