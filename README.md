# Bulk actions for Zotonic admin

This is a JavaScript-based drop-in module.

The module adds select options for lists:

* select this row
* select all rows
* select none

Select the item you want to change, then press the action button. 

Implemented commands:

* Delete
* Set 'published' state
* Set 'featured' state
* Set 'protected' state

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


## To do

* More actions
* Translations
* Categories page

