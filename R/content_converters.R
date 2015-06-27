#===================================================================================================
#' Make pandoc conversion functions
#' 
#' Makes generic pandoc file converter functions
#' 
#' @param block_header (named \code{character}) The name of the programming language, 
#' as it would appear in a fenced code block identifier. The names are the file extension for
#' files of that programming language.
#' 
#' @examples 
#' \dontrun{
#' # The following would make the function `quiltr_convert_python_to_html` and `quiltr_convert_py_to_html`
#' make_generic_pandoc_converter(c(py = "python", python = "python"))
#' }
make_generic_pandoc_converter <- function(block_header = pandoc_extension_identifiers()) {
  make_one <- function(language) {
    force(language)
    function(input, output = tempfile(fileext = ".html")) {
      if (pandoc_is_available()) {
        pandoc_command <- paste("pandoc", "-s", "--highlight-style pygments", "-o", output)
        content <- paste0("# ", basename(input), "\n\n", 
                          "```", language, "\n", readChar(input, nchars = 10000000), "\n```")
        system(pandoc_command, input = content)    
      } else {quiltr_convert_txt_to_html(input, output)}
      return(output)
    }
  }
  for (index in seq_along(block_header)) {
    function_name <- paste0("quiltr_convert_", names(block_header)[index], "_to_html")
    assign(function_name, make_one(block_header[index]), envir = parent.frame())
  }
}



#===================================================================================================
#' File extensions for pandoc highlighting
#' 
#' Returns the file extensions associated with pandoc fenced block identifiers.
#' 
#' @return named \code{character}. Names are file extensions and values are pandoc fenced block
#' identifiers.
pandoc_extension_identifiers <- function() {
  output <- c(as = "actionscript",
              adb = "ada",
              ads = "ada",
              asn1 = "asn1",
              asp = "asp",
              awk = "awk",
              bash = "bash", 
              sh = "bash",
              bibtex = "bibtex",
              boo = "boo",
              c = "c",
              clj = "clojure",
              cljs = "clojure",
              edn = "clojure",
              cmake = "cmake",
              coffee = "coffee",
              cfm = "coldfusion",
              cfc = "coldfusion",
              lisp = "commonlisp", 
              cl = "commonlisp",
              cpp = "cpp",
              cs = "cs",
              css = "css",
              curry = "curry",
              d = "d",
              diff = "diff",
              django.html = "djangotemplate",
              dtd = "dtd",
              e = "eiffel",
              eml = "email",
              erl = "erlang",
              hrl = "erlang",
              f = "fortran",
              "for" = "fortran",
              f90 = "fortran",
              f95 = "fortran",
              "f#" = "fsharp",
              fs = "fsharp",
              go = "go",
              hs = "haskell",
              lhs = "haskell",
              hx = "haxe",
              hxml = "haxe",
              html = "html",
              ini = "ini",
              java = "java",
              class = "java",
              jar = "java",
              js = "javascript",
              json = "json",
              jsp = "jsp",
              jl = "julia",
              tex = "latex",
              lex = "lex",
              lua = "lua",
              md = "markdown",
              m = "matlab",
              mac = "maxima",
              mc = "maxima",
              mf = "metafont",
              s = "mips",
              mod = "modula2",
              i3 = "modula3",
              m3 = "modula3",
              asm = "nasm",
              nw = "noweb",
              h = "objectivec",
              mm = "objectivecpp",
              ml = "ocaml",
              p = "pascal",
              pp = "pascal",
              lpr = "pascal",
              dpr = "pascal",
              pl = "perl",
              php = "php",
              pike = "pike",
              ps = "postscript",
              pro = "prolog",
              pd = "pure",
              py = "python",
              r = "r",
              rng = "relaxngcompact",
              rst = "restructuredtext",
              rhtml = "rhtml",
              roff = "roff",
              rb = "ruby",
              rs = "rust",
              scala = "scala",
              scm = "scheme",
              ss = "scheme",
              sci = "sci",
              sed = "sed",
              sgml = "sgml",
              sql = "sql",
              mysql = "mysql",
              tcl = "tcl",
              texi = "texinfo",
              v = "verilog",
              vhdl = "vhdl",
              vhd = "vhdl",
              xml = "xml",
              xslt = "xslt",
              xul = "xul",
              y = "yacc",
              yml = "yaml",
              yaml = "yaml")
  ambiguous <- output[sapply(names(output), function(x) sum(names(output) == x) > 1)]
  if (length(ambiguous) > 0) {
    stop("Duplicate file extension associations detected:\n\t",
         paste(names(ambiguous), ambiguous, sep = " : ", collapse = ", "))
  }
  return(output)
}


make_generic_pandoc_converter()



#===================================================================================================
#' Convert rmd to html
#' 
#' Convert rmd to html
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
quiltr_convert_rmd_to_html <- function(input, output = tempfile(fileext = ".html")) {
  original_wd <- getwd()
  on.exit(setwd(original_wd))
  setwd(dirname(input))
  rmarkdown::render(input, output_file = basename(output), output_dir = dirname(output), 
                    quiet = TRUE)
}


#===================================================================================================
#' Convert md to html
#' 
#' Convert md to html
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
quiltr_convert_md_to_html <- function(input, output = tempfile(fileext = ".html")) {
  quiltr_convert_rmd_to_html(input, output)
}


#===================================================================================================
#' Convert html to html
#' 
#' Convert html to html
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
quiltr_convert_html_to_html <- function(input, output = tempfile(fileext = ".html")) {
  file.copy(input, output)
  return(output)
}


#===================================================================================================
#' Convert txt to html
#' 
#' Convert text files to html
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
quiltr_convert_txt_to_html <- function(input, output = tempfile(fileext = ".html")) {
  content <- paste0("# ", basename(input), "\n\n", 
                    "```\n", readChar(input, nchars = 10000000), "\n```")
  cat(knitr::knit2html(text = content, output = output, quiet = TRUE), file = output)
  return(output)
}



#===================================================================================================
#' Convert pdf to html
#' 
#' Display pdf files via html embed
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
quiltr_convert_pdf_to_html <- function(input, output = tempfile(fileext = ".html")) {
  content <- paste0("<h1>", basename(input), "</h1>\n\n", 
                    '<embed src="', basename(input), 
                    '" width="100%" height="600px" type="application/pdf">')
  cat(content, file = output)
  return(output)
}


#===================================================================================================
#' Convert R to html
#' 
#' Convert R files to html
#' 
#' @param input (\code{character} of length 1)
#' @param output (\code{character} of length 1)
quiltr_convert_r_to_html <- function(input, output = tempfile(fileext = ".html")) {
  quiltr_convert_rmd_to_html(input, output)
}



