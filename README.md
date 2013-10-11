i18nize-templates
=================

A tool to automatically add i18n markup to jinja2 and handlebars
templates.  Given the similarity between jinja2 and django templates
(for these purposes), it may also work for django, though this is not
tested.

This is part of a process to make a non-i18n-aware jinja2 or
handlebars file i18n-aware.  i18n-ness support is mostly a matter of
marking natural-language text in the file that needs to be translated.
While some complicated natural language constructs (like plurals)
require a bit more work, the simple case is very simple: replace

    <p>Hello <b>world</b></p>

with

    <p>{{ _("Hello <b>world</b>") }}</p>

This script helps with that process.


Use
---

    i18nize_templates.py <file> ...

OR

    i18nize_templates.py [--handlebars] < <infile> > <outfile>

In the first use-case, the files are modified in-place.  In the second
use-case, infile is assumed to be a jinja2 template unless
`--handlebars` is specified.

### What it does ###

This script will replace runs of natural language text with a
'translation marker'.  Most template libraries have built in support
for some translation markers, notably `{% trans %}` and `_(...)`.  We
use *only* the second form; we never use `{% trans %}`.

### Correcting i18nize_templates mistakes ###

If the script marks some text for translation that should not have
been marked, you can wrap it in a special indicator:

    <input type="button" value="{{i18n_do_not_translate("x")}}">  # jinja2
    <input type="button" value="{{#i18nDoNotTranslate}}x{{/i18nDoNotTranslate}}">  # handlebars

If you want to use these special indicators, you will need to teach
your jinja2/handlebars environment about them, for instance:

    jinja2.default_config = {"globals": {"i18n_do_not_translate": lambda s: s}}
    Handlebars.registerHelper("i18nDoNotTranslate", function(options) { return options.fn(this); })


Use as a library
----------------

At Khan Academy, there are several places we've used i18nize_templates
as a library, for when we wanted to work just on the natural-language
components of an html file.

Here's an example function that wraps natural-language text in a span.
Then you can write javascript to, say, pop up a translation dialog
when the span is clicked on.  This example purposefully skips natural
text in tag attributes, since you can't wrap tag attributes in spans.

```
import i18nize_templates

def spanify(file_contents):
    """Parse the input text into natural-language runs, and wrap each run.

    We put a single span tag around each run.  The idea is that the
    run will be presented as a single unit to the translator (via
    javascript magic).  Natural-language attributes in tags are
    left alone, since you can't put <span>s inside tags.
    """
    def parser_callback(segment, segment_separates_nltext):
        if segment is None:
            return ''               # called at end-of-parse
        elif (segment_separates_nltext
              or (segment.startswith('<') and segment.endswith('>'))):
            return segment   # do not translate
        else:
            return '<span class="nltext">%s</span>' % segment

    parser = i18nize_templates.HtmlLexer(parser_callback)
    return parser.parse(file_contents)
```

Here's another example program that uses i18nize-templates as a
library to auto-translate an html file to box-language.  In this case,
we *do* translate natural-language tag attributes.

```
import re, i18nize_templates

def boxify(file_contents):
    """Parse the input text into 'chunks', and translate each chunk.

    The input is chunked by tags.  Natural-language attributes in tags
    are translated but otherwise tags are left alone; everything else
    is fair game for boxification.
    """
    def parser_callback(segment, segment_separates_nltext):
        if segment is None:
            return ''               # called at end-of-parse
        elif segment.startswith('<') and segment.endswith('>'):
            # A handler that doesn't do anything itself, but when it
            # sees a tag attribute that needs translation, it calls
            # the given parser to translate it.
            tag_handler = i18nize_templates.NullTextHandler(
               i18nize_templates.HtmlLexer(parser_callback))
            return tag_handler.handle_tag(segment)
        elif (segment_separates_nltext
              or (segment.startswith('{{') and segment.endswith('}}'))):
            return segment   # do not translate
        else:
            return re.sub(r'\w', u'\u25a1', segment)     # a hollow box

    # Could also use Jinja2HtmlLexer (which is why we bothered to
    # check for '{{' above).
    parser = i18nize_templates.HtmlLexer(parser_callback)
    return parser.parse(file_contents)
```

In general, you will want to use a standard parser -- `HtmlLexer`,
`Jinja2HtmlLexer`, or `HandlebarsHtmlLexer` -- and write your own text
handler.  See the 'boxify' example above if you want your text handler
to properly handle natural-language text inside tags.  (Your text
handler will see the tag as a single segment, and will need to make a
second parse call to translate text inside it.)


Customizing the library
-----------------------

When you use i18nize_templates as a library, you can customize it via
the following function calls:

##### `i18nize_templates.add_nltext_tag_attribute("span", "myapp-description")` #####

If you have your own application-specific html tag attributes that
hold natural language text, you can tell this script about them via
this function call.  For example, i18nize_templates will know that
`xxx` is natural langauge text in `<span myapp-description="xxx">`.

##### `i18nize_templates.add_nltext_separator_class("separator")` #####

Likewise, if you have application-specific classes that separate
natural language text, you can specify them via
`add_nltext_separator_class()`.  Note this function takes a regular
expression string, not a literal text string.

This means that `a<br class="highlight separator"></span>b` will be
treated as two different nltext runs, even though normally a `br` tag
would not separate runs.

##### `i18nize_templates.mark_function_args_lack_nltext("log.date.strftime")` #####

This is used for function calls, or filters, made from within django
or jinja2 templates.  This script assumes that these function calls
have natural-language text as arguments, so when it sees literal
strings for these arguments, it marks them for translation.  If you
know that the function takes only arguments that do not need to be
translated, you can call `mark_function_args_lack_nltext()` on it.


Caveats
-------

This tool makes no pretentions toward perfection.  For situations
where it's pretty sure there's natural language text but it doesn't
know how to handle it, it will insert text like this:

    <p>{{ _TODO(some_function_call("Hello {{world}}")) }}</p>

This will require manual cleanup before the template file is usable.

This script will not capture all natural-language text in an html file
or template.  For instance, a manual pass will still be needed to
detect natural-language text inside

1. javascript `<script>` tags
2. jinja2 `{% set %}` directives
3. ...

This script will never insert `ngettext` markup.  If your original
text had logic to handle plurals, you will need to modify the
marked-up text by hand to be more correct for i18n:

Original text:

    {% if num == 1 %}I have one apple{% else %}I have {{num}} apples{% endif %}

Marked up text:

    {% if num == 1 %}{{ _("I have one apple") }}{% else %}{{ _("I have %(num)s apples", num=num) }}{% endif %}

What you should manually change the text to:

    {{ ngettext("I have one apple", "I have %(num)s apples", num) }}
