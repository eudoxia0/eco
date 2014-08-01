# Eco - Fast, flexible, designer-friendly templates.

# Goals

- **Simple:** Eco is essentially just a string concatenator. It introduces no
  constructs of its own: Every tag is pure Common Lisp code, plain and simple.
- **Easy to Use:** Proper use of Eco should not require more than a
  cursory read of this README from time to time.
- **Designer-friendly:** Lispers have written template engine after template
  engine that turns S-expressions into HTML. Which is great, if you're a
  lisper. Eco is meant to be used for more than HTML and also to be usable to
  the many designers and programmers who don't know the language and should not
  be expected to learn an obscure template syntax to be able to contribute.
- **Performance:** Eco uses the many performance advantages of Common
  Lisp. Templates are not interpreted or run in a VM, but compiled to Common
  Lisp, which is then compiled down to efficient machine code. By making each
  template a function that takes an (Optionally typed) argument list rather than
  passing an environment hash table like most other template engines, one can
  leverage the type inference features of modern Common Lisp implementations to
  create performant templates.

# Usage

A basic template (`.eco` extension) looks like this:

```html
@template index title &optional posts {
  <!DOCTYPE html>
  <html>
    <head>
      <title>@{title}</title>
    </head>
    <body>
      @if posts {
        <h1>Recent Posts</h1>
        <ul id="post-list">
          @loop for (title . snippet) in posts {
            <li>@{title} - @{title}</li>
          }
        </ul>
      } {
        <span>No recent posts.</span>
      }
    </body>
  </html>
}
```

To load this template, put this in your system definition file:

```lisp
(:eco-template "filename")
```

To execute the template:

```lisp
(eco-template:index "My Blog" nil)
```

**Note:** Eco is designed to be output-agnostic, so by default it will **not**
autoescape HTML. Use the `e` function for that. You have been warned.

# Tags

- `@{<expr>}` becomes `(print-object <expr>)`.
- `@<expr>{ <body> }` becomes `(print-object (<expr> (emit <body>)))`.

# Options

- `*autoescape*`: Automatically escape all expressions. Defaults to `NIL`.
- `*template-package*`: The package the templates will be defined it. Defaults
  to `:eco-template`.

# Reference

## `template`

**Syntax:**

```html
@template <name> <arguments>* {
  <body>
}
```

Defines a named template.

# Examples

## `if`

**Syntax:**

```html
@if test {
  <body>
}

@if test {
  @{
    <true-branch>
  }
  @{
    <false-branch>
  }
}
```

# Implementation

Eco uses esrap to parse templates, which it then compiles down to Common Lisp
code.

# License

Copyright (c) 2014 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
