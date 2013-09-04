UrEmbed
-------

Embeds file(s) into the Ur/Web project by creating the module containing

    datatype content = File_A | File_B ...
    val binary : content -> transaction blob

Additionally, Urembed greatly simplifies writing of the JavaScript FFIs.

Installation
------------

You will need [Haskell platform](http://www.haskell.org/platform/) and several
additional packages. Also, POSIX environment is required (Cygwin should suffice
under Windows)

    $ git clone https://github.com/grwlf/urembed
    $ cd urembed
    $ cabal configure
    $ cabal build
    $ cabal install

Usage
-----

    $ urembed --help
    UrEmebed is the Ur/Web module generator

    Usage: urembed (-o|--output FILE.urp) [-I|--urinclude DIR] [FILE]
      Converts a FILE to the Ur/Web's module. The Module will contain a 'binary' 
      function returning the FILE as a blob. 
     
      Example: urembed -o static/Static.urp Style.css Script.js
      urembed honores CC and LD env vars used to call the C compiler and linker
      respectively


    Available options:
      -h,--help                Show this help text
      -o,--output FILE.urp     Target Ur project file
      -I,--urinclude DIR       Custom location of the UrWeb's includes
      FILE

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
    val init_unit : css_class -> string -> transaction unit

