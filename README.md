# ABAP W3MI poller
A tool to poll file change and upload changed files automatically as W3MI object.
Created mainly to simplify CSS editing in https://github.com/abapGit/abapGit, but can be used for other purposes.

In particular, can be used as a **library** of file system and mime object tools. Which I actually do in my other project e.g. [mockup loader](https://github.com/sbcgua/mockup_loader) and [mockup compiler](https://github.com/sbcgua/mockup_compiler). See more details below.

![logo](https://github.com/sbcgua/abap_w3mi_poller/wiki/img/w3mipoller-logo.png)

## Installation

Clone the repository to your SAP system using [abapGit](https://github.com/abapGit/abapGit) tool.

## Usage
Enter filename to poll and target object at selection screen. You can also choose multiple polling pairs. The first pair is remembered in user parameters for the session live time.

Optionally, choose if you want to upload the file to SAP or replace the fronend file before the polling start.

![Selection screen](https://github.com/sbcgua/abap_w3mi_poller/wiki/img/selscreen.png)

![Poller](https://github.com/sbcgua/abap_w3mi_poller/wiki/img/poller.png)

## Use as library

The package contains several useful routines grouped into classes. All routines are static methods. Some of them may raise `zcx_w3mime_error` exception (use `get_text` to get some description of an error). For more details see implementations directly - the routines are mostly simple convenient wrappers over existing SAP calls. Might happen that I invented bicycle or a couple - sorry if so :)

- zcl_w3mime_storage
  - `read_object(_x)` - read MIME object into raw255 table (or xstring)
  - `update_object(_x)` - save raw255 table (or xstring) as a MIME object (must exist already)
  - `choose_mime_dialog` - display dialog to choose MIME object. Can be used for F4 search helps. Filters only `Z` objects.
  - minor utils like `get_object_info` and `check_obj_exists`
- zcl_w3mime_fs
  - `choose_dir_dialog`, `choose_file_dialog` - quick wrappers around cl_gui_frontend methods.
  - `read_file(_x)`, `write_file(_x)` - quick wrapper to read/write a file, raw255 and xstring versions respectfully
  - `parse_path` - split path into directory, filename and extension
  - `resolve_filename` - split path into directory and filename, if no directory - fallback to SAP GUI default path
  - `path_join` - join 2 path parts e.g. directory and filename, respecting separators (but ignoring `..` at the moment)
  - `path_is_relative` - detects if one path is relative to the other (but ignoring `..` at the moment)
  - `path_relative` - calculates the difference between 2 paths (but ignoring `..` at the moment)
  - `path_ensure_dir_tail` - ensures dir ends with separator symbol
- zcl_w3mime_utils
  - `download` - save W3MIME object directly to file
  - `upload` - vice versa
- zcl_w3mime_poller
  - most complex wrapper. Polls directory (can be multiple) once per given period of time for file changes. If detected fires `changed` event with changed file list.
  - Usage: `construct` the object (this one is not static) giving the target list and timer interval, assign the event handler to `changed` (and to `error` optionally), `start` the poller.
  - see `zw3mimepoll` program if you are looking for an example. see `lcl_poller` class implementation - it contains concrete handling of a more abstract `zcl_w3mime_poller` tool
- zcl_w3mime_zip_writer
  - a wrapper around `cl_abap_zip`.
  - `has` - checks of the file exists in zip
  - `add(x), read(x)` - reads write data from zip, methods without `x` convert the data to string based on encoding specified during instantiation
  - `get_blob` - returns xstring of current zip state
  - `is_dirty` - tracks if there has been any changes since last `get_blob`. (`add` sets the flag, `get_blob` releases it).
