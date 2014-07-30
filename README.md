# Eco - Fast, flexible, designer-friendly templates.

# Usage

A basic template (`.eco` extension) looks like this:

```html
{% template index title &optional posts %}
  <!DOCTYPE html>
  <html>
    <head>
      <title><% title %></title>
    </head>
    <body>
      {% if posts %}
        <h1>Recent Posts</h1>
        <ul id="post-list">
          {% loop for (title . snippet) in posts %}
            <li><% title %> - <% snippet %></li>
          {% end %}
        </ul>
      {% else %}
        <span>No recent posts.</span>
      {% end %}
    </body>
  </html>
{% end %}
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

- `<% [expr] %>` becomes `(print-object [expr])`.
- `{% tag [expr] %}[body]{% end %}` becomes `(tag [expr] [body])`, with the
  exception of the `if` statement.

# Options

- `*autoescape*`: Automatically escape all expressions. Defaults to `NIL`.
- `*template-package*`: The package the templates will be defined it. Defaults
  to `:eco-template`.

# License

Copyright (c) 2014 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
