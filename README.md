# Bulk actions for Zotonic admin

Select one or more items from the list, then perform an action. Implemented commands:

- Delete
- Change state attributes: published, featured, protected

## Checks

- ACL rights are maintained.
- It is not possible to delete a page that is protected.

## Installation

Zotonic `>= 0.10`:

    zotonic modules install mod_bulk_commands

Zotonic `>= 0.7`:

    zotonic installmodule mod_bulk_commands

Zotonic `<= 0.6`:

    git clone https://github.com/ArthurClemens/mod_bulk_commands.git mod_bulk_commands

Activate the module in Admin > System > Modules.

## Configuration

At activation, 2 config values are set:

- `mod_bulk_commands.pages` contains a comma-separated list of page names where the module should appear; default value: `admin_overview_rsc,admin_media`
- `mod_bulk_commands.commands` contains a comma-separated list of command names to appear in the interface; default value: `delete,published,featured,protected`

Change the config values to your requirements.

## Requirements

JavaScript should be enabled in the browser.
