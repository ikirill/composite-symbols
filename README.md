# Composite special symbols for programming

This mode replaces some special symbols with corresponding unicode
characters. For example, it will replace "<=" with <LESS-THAN> '<',
and "None" with <EMPTY SET> '∅' (with default settings, which can
be modified).

## To use

- `composite-symbols-mode` enables special symbols in some default
  programming languages. If your language is not supported, open an
  issue on github, or define your own configuration. If the mode
  replaces symbols that you do not want to see replaced, you can
  configure your own replacement rules.
- `composite-symbols-greek-mode` shows all non-ambiguous Greek
  letters that are spelled in English as the corresponding Greek
  letters.
- `composite-symbols-ignore-indentation` controls whether the mode
  is allowed to "break" indentation by changing lengths of lines.
- For a starting point configuring your own replacement rules, look
  at the definition of `composite-symbols-assign-arrow-mode`.

## Screenshot

This is an artificial testing C++ file. When symbols change
their lengths, indentation is, by default, not broken.

![Screenshot](screenshot.png?raw=true "Screenshot")

### Notes:

- There is a fair amount of variability in what you might prefer to
  see from this package.  See definitions of variables like
  `composite-symbols-cc-rules` to see how to define your own sets of
  composite symbol replacements.
- Many of these characters might be missing from your default
  fonts.  It may be helpful to install the package `unicode-fonts`,
  or use set-fontset-font to update which font Emacs uses to
  display which characters (other, non-default fonts might have the
  missing characters).

  The package unicode-fonts includes helpful suggestions about
  which extra fonts might need to be installed.

- When defining new composite symbols, the mode
  `show-unicode-minor-mode` is quite helpful as it shows next to
  every hexadecimal literal the corresponding Unicode character.
- This (programming-language-agnostic) package derives from the
  code that haskell-mode uses to fontify haskell code.
- font-lock suppresses errors in the fontification process.  The
  package `font-lock-studio` can make debugging easier.
