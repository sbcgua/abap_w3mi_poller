# ABAP W3MI poller
A tool to poll file change and upload changed files automatically as W3MI object.
Created mainly to simplify CSS editing in https://github.com/larshp/abapGit, but can be used for other purpose too.

![logo](https://github.com/sbcgua/abap_w3mi_poller/wiki/img/w3mipoller-logo.png)

## Installation

* The best option to install is to clone the repository to your SAP system using [abapGit](https://github.com/larshp/abapGit) tool.
* Otherwise, create program (SE38) `ZW3MIMEPOLL` and copy the content of `zw3mimepoll.prog.abap` file to it. Also create user parameters `ZW3MIMEPOLL_FILE` and `ZW3MIMEPOLL_OBJ` (SE80 => Other objects => Other => SET/GET Parameters or SM30 on TPARA table).

## Usage
Enter filename to poll and target object at selection screen. You can also choose multiple polling pairs. The first pair is remembered in user parameters for the session live time.

Optionally, choose if you want to upload the file to SAP or replace the fronend file before the polling start.

## Screenshots

![Selection screen](https://github.com/sbcgua/abap_w3mi_poller/wiki/img/selscreen.png)

![Poller](https://github.com/sbcgua/abap_w3mi_poller/wiki/img/poller.png)
