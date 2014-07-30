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
          {% endloop %}
        </ul>
      {% else %}
        <span>No recent posts.</span>
      {% endif %}
    </body>
  </html>
{% endtemplate %}
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
- `{% tag [expr] %}[body]{% endtag %}` becomes `(tag [expr] [body])`, with the
  exception of the `if` statement.

# Options

- `*autoescape*`: Automatically escape all expressions. Defaults to `NIL`.
- `*template-package*`: The package the templates will be defined it. Defaults
  to `:eco-template`.

# Reference

## `template`

**Syntax:**

```html
{% template <name> <arguments>* %}
  <body>
{% endtemplate %}
```

## `if`

**Syntax:**

```html
{% if <test> %}
  <true-branch>
{% else %}
  <false-branch>
{% endif %}

# License

Copyright (c) 2014 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
