UrEmbed
-------

Embeds a file as a blob to the Ur/Web project


Installation
------------

You will need Haskell Platform and several additional packages. Also, POSIX
environment is required (Cygwin should suffice under Windows)

    $ git clone https://github.com/grwlf/urembed
    $ cd urembed
    $ cabal install

Usage
-----

To embed Style.css into Ur/Web module, type

    $ urembed -o lib/autogen/Style_css.urp Style.css

From the Ur/Web project you will need to add Style\_css library and use the
provided function

    # Style_css.urs
    val binary : unit -> transaction blob

Particularly, we now can serve the blob to the user:

    # Main.ur
    fun serve_css a =
      b <- Style_css.binary ();
      returnBlob b (blessMime "text/css")

Additionally, urembed is able to bind top-level JavaScript functions via
JavaScript FFI. In order to do it, user has to make sure that FILE has .js
extention and contains top-level functions named according to the 'name\_type'
format. For example:
    
    # FILE.js
    function init_unit(menustyle_css_class, text_string) {}

will be binded to Ur/Web's function

    # FILE.urs
    val init_fun : css_class -> string -> transaction unit

