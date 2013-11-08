# 12tone-table - making pdf files of all 48 variants of a twelve tone row #

This a small project for creating pdf files of all 48 variants of a twelve tone row. It depends on [LilyPond](http://www.lilypond.org/).

## Usage ##

```
12tone-table [--ly PATH] [--row "ROW"] [--paper FORMAT] [--help] --out PATH
Creating PDF files with a table of all 48 variants of a twelve tone row. lilypond is required.

Arguments
   --ly     path to lilypond (default: /Applications/LilyPond.app/Contents/Resources/bin/lilypond)
   --row    twelve tone row (default: "bes e ges ees f a d cis g gis b c")
   --paper  A4 or C4 (default: C4)
   --help   displays this help
   --out    path to pdf file to be created
```

## Build your own executable ##

At the moment I provide a executable for Mac OS X. You can build your own executable with
* [SBCL](http://www.sbcl.org/) (better build with :SB-CORE-COMPRESSION) and
* [buildapp](http://www.xach.com/lisp/buildapp/).

Now you can build your executable:
`buildapp --compress-core --output 12tone-table --load 12tone.lisp --entry main`

## License ##

This software is licensed under the [CC BY 3.0 license](http://creativecommons.org/licenses/by/3.0/).

## Contact ##

I am happy if you share your ideas with me: <frank_zalkow@web.de>.
