UrEmbed
-------

Embeds file(s) into the Ur/Web project by creating the module containing

    datatype content = File_A | File_B ...
    val binary : content -> transaction blob

Additionally, Urembed greatly simplifies writing of the JavaScript FFIs.

Installation
------------

You will need Haskell Platform and several additional packages. Also, POSIX
environment is required (Cygwin should suffice under Windows)

    $ git clone https://github.com/grwlf/urembed
    $ cd urembed
    $ cabal install

Usage
-----

### Static files

To embed Style.css into Ur/Web module, type

    $ urembed -o lib/autogen/Static.urp Style.css

From the Ur/Web project you will need to add Static in your main .urp library
and use the binary function:

    # Static.urs
    datatype content = Style_css
    val binary : content -> transaction blob

Particularly, that is how to serve the blob to the user:

    # Main.ur
    fun serve_css a =
      b <- Static.binary Static.Style_css;
      returnBlob b (blessMime "text/css")

### JavaScript FFI helper

Urembed is able to bind top-level JavaScript functions via
JavaScript FFI. In order to do it, user has to make sure that FILE has .js
extension and contains top-level functions named according to the 'name\_type'
format. For example:
    
    # FILE.js
    function init_unit(menustyle_css_class, text_string) {}

will be binded to Ur/Web's function

    # FILE.urs
    val init_fun : css_class -> string -> transaction unit

