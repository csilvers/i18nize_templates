#!/usr/bin/env python

"""Unitest for i18nize_templates.py."""

import sys
import unittest

# This makes it so we can find i18nize_templates when running from repo-root.
sys.path.insert(1, '.')

import i18nize_templates


class HtmlTest(unittest.TestCase):
    """Test some simple html parsing."""
    def check(self, input):
        text_handler = i18nize_templates.NullTextHandler
        tag_parser = i18nize_templates.HtmlLexer(
            text_handler(None).handle_segment)
        parser = i18nize_templates.HtmlLexer(
            text_handler(tag_parser).handle_segment)
        actual = parser.parse(input)
        self.assertEqual(input, actual)

    def test_non_existent_tag(self):
        self.check('this <is a tag>.')

    def test_fake_tag(self):
        self.check('<http://www.khanacademy.org>')

    def test_error_after_init(self):
        parser = i18nize_templates.HtmlLexer(None)
        try:
            parser.error("test")
        except Exception, e:
            self.assertEqual("test, at line 1, column 1", "%s" % e)


class Jinja2Test(unittest.TestCase):
    def check(self, input, expected):
        text_handler = i18nize_templates.Jinja2TextHandler
        tag_parser = i18nize_templates.Jinja2HtmlLexer(
            text_handler(None).handle_segment)
        parser = i18nize_templates.Jinja2HtmlLexer(
            text_handler(tag_parser).handle_segment)
        actual = parser.parse(input)
        self.assertEqual(expected, actual)

    def todo(self, input, expected):
        pass

    def test_simple(self):
        self.check('Hello, world.', '{{ _("Hello, world.") }}')

    def test_empty(self):
        self.check('', '')

    def test_no_text(self):
        self.check('!@#$@!#$', '!@#$@!#$')

    def test_inline_tag(self):
        self.check('You are <b>bold</b>', '{{ _("You are <b>bold</b>") }}')

    def test_trailing_punctuation_after_tag(self):
        self.check('You are <b>bold</b>.', '{{ _("You are <b>bold</b>.") }}')
        self.check('<b>.</b>', '<b>.</b>')
        self.check('<a href="foo">hello <b>there</b></a>',
                   '<a href="foo">{{ _("hello <b>there</b>") }}</a>')

    def test_leading_punctuation_to_match_trailing_punctuation(self):
        self.check('(You are <b>bold</b>)',
                   '{{ _("(You are <b>bold</b>)") }}')
        self.check('"You are <b>bold</b>."',
                   '{{ _("\\"You are <b>bold</b>.\\"") }}')

    def test_block_tag(self):
        self.check('You are <p>bold</p>.',
                   '{{ _("You are") }} <p>{{ _("bold") }}</p>.')

    def test_br_br_is_a_block_tag(self):
        self.check('You are<br><br>bold.',
                   '{{ _("You are") }}<br><br>{{ _("bold.") }}')
        self.check('You are<br/><br/>bold.',
                   '{{ _("You are") }}<br/><br/>{{ _("bold.") }}')

    def test_separator_classes_are_a_block_tag(self):
        self.check('You <span>are</span> bold<span class="button">ok</span>',
                   '{{ _("You <span>are</span> bold") }}'
                   '<span class="button">{{ _("ok") }}</span>')

    def test_leading_and_trailing_tags_in_segment(self):
        self.check('<a href="#"><span>Text<span>More text</span></span>',
                   '<a href="#"><span>{{ _("Text<span>More text</span>") }}'
                   '</span>')

    def test_br_tag_with_leading_and_trailing_tags_in_segment(self):
        self.check('<span>Text<br><span>More text</span></span>',
                   '<span>{{ _("Text<br><span>More text</span>") }}</span>')

    def test_jinja2_variable(self):
        self.check('You are {{bold}}',
                   '{{ _("You are %(bold)s", bold=bold) }}')
        self.check('You are {{  bold }}',
                   '{{ _("You are %(bold)s", bold=bold) }}')
        self.todo('You are {{bold["ness"]}}',
                  '{{ _("You are %(bold__ness__)s",'
                  ' bold__ness__=bold["ness"]) }}')

    def test_only_jinja2_variable(self):
        self.check('{{bold}}', '{{bold}}')
        self.check('{{  bold }}', '{{  bold }}')
        self.check('{{bold["ness"]}}', '{{bold["ness"]}}')

    def test_jinja2_comment(self):
        self.check('You are {# make bold #}bold',
                   '{{ _("You are") }} {# make bold #}{{ _("bold") }}')

    def test_jinja2_block(self):
        self.check('You are {% if bold %}bold{%else%}timid{%endif%}',
                   '{{ _("You are") }} {% if bold %}{{ _("bold") }}'
                   '{%else%}{{ _("timid") }}{%endif%}')

    def test_jinja2_variable_in_tag(self):
        self.check('You are <b {{how_bold}}>bold</b>',
                   '{{ _("You are <b %(how_bold)s>bold</b>",'
                   ' how_bold=how_bold) }}')

        self.check('You are <b {{ how_bold    }}>bold</b>',
                   '{{ _("You are <b %(how_bold)s>bold</b>",'
                   ' how_bold=how_bold) }}')

        self.check('You are <b {{how_bold}} id=a>bold</b>',
                   '{{ _("You are <b %(how_bold)s id=a>bold</b>",'
                   ' how_bold=how_bold) }}')

        self.check('You are <b id=a {{how_bold}}>bold</b>',
                   '{{ _("You are <b id=a %(how_bold)s>bold</b>",'
                   ' how_bold=how_bold) }}')

        self.check('You are <b id=a {{how_bold}} name=b>bold</b>',
                   '{{ _("You are <b id=a %(how_bold)s name=b>bold</b>",'
                   ' how_bold=how_bold) }}')

    def test_jinja2_conditional_attr_in_tag(self):
        self.check('You are <b {%if bold%}very{%endif%}>bold</b>',
                   '{{ _("You are <b {%if bold%}very{%endif%}>'
                   'bold</b>") }}')

        self.check('You are <b {% if bold  %}very{%  endif %}>bold</b>',
                   '{{ _("You are <b {% if bold  %}very{%  endif %}>'
                   'bold</b>") }}')

        self.check('You are <b foo=1 {%if bold%}very{%endif%} baz=2>bold</b>',
                   '{{ _("You are <b foo=1 {%if bold%}very{%endif%} baz=2>'
                   'bold</b>") }}')

        self.check('You are <b foo=1{%if bold%} very{%endif%} baz=2>bold</b>',
                   '{{ _("You are <b foo=1{%if bold%} very{%endif%} baz=2>'
                   'bold</b>") }}')

        # TODO(csilvers): this gives a parse error now.
        self.todo('You are <b foo=1 {%if bold%}very {%endif%}baz=2>bold</b>',
                  '{{ _("You are <b foo=1 {%if bold%}very {%endif%}baz=2>'
                  'bold</b>") }}')

        self.check('You are <b {%if bold%}very{%else%}not{%endif%}>bold</b>',
                   '{{ _("You are <b {%if bold%}very{%else%}'
                   'not{%endif%}>bold</b>") }}')

        self.check('You are <b{%if bold%} very{%else%} not{%endif%}>bold</b>',
                   '{{ _("You are <b{%if bold%} very{%else%}'
                   ' not{%endif%}>bold</b>") }}')

    def test_handle_nested_quotes(self):
        self.check('<a href="foo">hello</a> there',
                   '{{ _("<a href=\\"foo\\">hello</a> there") }}')

    def test_handle_containing_tags(self):
        self.check('<a href="foo">hello</a>',
                   '<a href="foo">{{ _("hello") }}</a>')

    def test_handle_leading_trailing_nonpunct(self):
        self.check('   hello  ', '   {{ _("hello") }}  ')

        self.check('<a href="foo">   hello  </a>',
                   '<a href="foo">   {{ _("hello") }}  </a>')

    def test_handle_some_containing_tags(self):
        self.check('<b><a href="foo">hello</a> there</b>',
                   '<b>{{ _("<a href=\\"foo\\">hello</a> there") }}</b>')

    def test_escape_pct(self):
        self.check('You are 100% {{name}}',
                   '{{ _("You are 100%% %(name)s", name=name) }}')
        self.check('You are 100%',
                   '{{ _("You are 100%") }}')

    def test_fake_script_tag(self):
        # A 'fake' script tag is one with type 'text/html'.
        # First we verify the 'normal' script tag handling.
        self.check('<script>This is ignored</script>',
                   '<script>This is ignored</script>')
        self.check('<script type="text/javascript">This is ignored</script>',
                   '<script type="text/javascript">This is ignored</script>')

        self.check('<script type="text/html">This is not ignored</script>',
                   '<script type="text/html">{{ _("This is not ignored") }}'
                   '</script>')

    def test_escape_tag_attributes(self):
        self.check('<input value="translated" type=button>',
                   '<input value="{{ _("translated") }}" type=button>')
        self.check('<input value="translated" type="button">',
                   '<input value="{{ _("translated") }}" type="button">')
        self.check('<input type="button" value="translated">',
                   '<input type="button" value="{{ _("translated") }}">')

    def test_escape_wildcard_tag_attributes(self):
        self.check('<foo title="translated">',
                   '<foo title="{{ _("translated") }}">')

    def test_escape_tag_attributes_preceded_by_newline(self):
        self.check('<p>\n<input type="button" value="translated">',
                   '<p>\n<input type="button" value="{{ _("translated") }}">')

    def test_escape_tag_attributes_with_single_quotes(self):
        self.check('<input value="john\'s input" type=button>',
                   '<input value="{{ _("john\'s input") }}" type=button>')

    def test_escape_tag_attributes_only_jinja2_var(self):
        self.check('<p>\n<input type="button" value="{{translator}}">',
                   '<p>\n<input type="button" value="{{translator}}">')
        self.check('<p>\n<input type="button" value="{{translator["new"]}}">',
                   '<p>\n<input type="button" value="{{translator["new"]}}">')

    def test_escape_tag_attributes_with_jinja2_vars(self):
        self.check('<input value="translated by {{author}}!" type=button>',
                   '<input value="{{ _("translated by %(author)s!",'
                   ' author=author) }}" type=button>')

    def test_escape_tag_attributes_with_jinja2_block(self):
        self.check('<input value={%if x%}"a"{%else%}"b"{%endif%} type=button>',
                   '<input value={%if x%}"{{ _("a") }}"{%else%}'
                   '"{{ _("b") }}"{%endif%} type=button>')

        self.check("<input value={%if x%}'a'{%else%}'b'{%endif%} type=button>",
                   "<input value={%if x%}'{{ _(\"a\") }}'{%else%}"
                   "'{{ _(\"b\") }}'{%endif%} type=button>")

        self.check('<input value="{%if x%}a{%else%}b{%endif%}" type=button>',
                   '<input value="{%if x%}{{ _("a") }}{%else%}'
                   '{{ _("b") }}{%endif%}" type=button>')

        self.check("<input value='{%if x%}a{%else%}b{%endif%}' type=button>",
                   "<input value='{%if x%}{{ _(\"a\") }}{%else%}"
                   "{{ _(\"b\") }}{%endif%}' type=button>")

    def test_do_not_escape_tag_attrs_in_nltext_tags(self):
        self.check('Hi <IMG title="Not translated"> there',
                   '{{ _("Hi <IMG title=\\"Not translated\\"> there") }}')
        self.check('<IMG title="Not translated"> there',
                   '<IMG title="{{ _("Not translated") }}"> {{ _("there") }}')

    def test_nltext_tag_outside_translation(self):
        self.check('<IMG title="all alone">',
                   '<IMG title="{{ _("all alone") }}">')

    def test_do_not_escape_tag_attrs_without_prereqs(self):
        self.check('<input value="Not translated">',
                   '<input value="Not translated">')
        self.check('<input value="Not translated" type="radio">',
                   '<input value="Not translated" type="radio">')

    def test_no_double_escaping(self):
        self.check('<p>{{ _("Hello, world") }}</p>',
                   '<p>{{ _("Hello, world") }}</p>')

        self.check('<p>{{_("Hello, world")}}</p>',
                   '<p>{{_("Hello, world")}}</p>')

        self.check('<p>{{ ngettext("Hello, 1 world", '
                   '"Hello, %(num)d worlds", num_worlds) }}</p>',
                   '<p>{{ ngettext("Hello, 1 world", '
                   '"Hello, %(num)d worlds", num_worlds) }}</p>')

        self.check('{{ fn(_("foo")) }}', '{{ fn(_("foo")) }}')
        self.check('{{ fn(ngettext("foo", "foos", 1)) }}',
                   '{{ fn(ngettext("foo", "foos", 1)) }}')

    def test_non_alnum_entities(self):
        self.check('<p>&amp;</p>', '<p>&amp;</p>')
        self.check('<p>&#215;</p>', '<p>&#215;</p>')
        self.check('<p>&#x20;</p>', '<p>&#x20;</p>')
        self.check('<p>&#X7e;</p>', '<p>&#X7e;</p>')
        self.check('<p>&#X3D;</p>', '<p>&#X3D;</p>')
        self.check('<p>&#2150;</p>', '<p>{{ _("&#2150;") }}</p>')

    def test_todo_leaves_vars_alone(self):
        self.check('This {{smurf}} is {{smurf|escape}} {{smurf}}!',
                   '_TODO(This {{smurf}} is {{smurf|escape}} {{smurf}}!)')

    def test_quote_in_functions_gives_todo(self):
        self.check('This {{ fn("hello") }} has quotes!',
                   '_TODO(This {{ fn("hello") }} has quotes!)')
        self.check('{{ fn("hello") }}',
                   '_TODO({{ fn("hello") }})')

    def test_empty_quote_does_not_give_todo(self):
        self.check('{{ fn("") }}',
                   '{{ fn("") }}')
        self.check('{{ fn(variable or "") }}',
                   '{{ fn(variable or "") }}')
        self.check('{{ fn("", "arg") }}',
                   '_TODO({{ fn("", "arg") }})')

    def test_do_not_translate_function(self):
        self.check('{{ i18n_do_not_translate("same in all languages") }}',
                   '{{ i18n_do_not_translate("same in all languages") }}')


class HandlebarsTest(unittest.TestCase):
    def check(self, input, expected):
        text_handler = i18nize_templates.HandlebarsTextHandler
        tag_parser = i18nize_templates.HandlebarsHtmlLexer(
            text_handler(None).handle_segment)
        parser = i18nize_templates.HandlebarsHtmlLexer(
            text_handler(tag_parser).handle_segment)
        actual = parser.parse(input)
        self.assertEqual(expected, actual)

    def todo(self, input, expected):
        pass

    def test_simple(self):
        self.check('Hello, world.', '{{#_}}Hello, world.{{/_}}')

    def test_empty(self):
        self.check('', '')

    def test_no_text(self):
        self.check('!@#$@!#$', '!@#$@!#$')

    def test_inline_tag(self):
        self.check('You are <b>bold</b>', '{{#_}}You are <b>bold</b>{{/_}}')

    def test_trailing_punctuation_after_tag(self):
        self.check('You are <b>bold</b>.', '{{#_}}You are <b>bold</b>.{{/_}}')
        self.check('<b>.</b>', '<b>.</b>')
        self.check('<a href="foo">hello <b>there</b></a>',
                   '<a href="foo">{{#_}}hello <b>there</b>{{/_}}</a>')

    def test_block_tag(self):
        self.check('You are <p>bold</p>.',
                   '{{#_}}You are{{/_}} <p>{{#_}}bold{{/_}}</p>.')

    def test_br_br_is_a_block_tag(self):
        self.check('You are<br><br>bold.',
                   '{{#_}}You are{{/_}}<br><br>{{#_}}bold.{{/_}}')
        self.check('You are<br/><br/>bold.',
                   '{{#_}}You are{{/_}}<br/><br/>{{#_}}bold.{{/_}}')

    def test_handlebars_variable(self):
        self.check('You are {{bold}}',
                   '{{#_}}You are {{bold}}{{/_}}')
        self.check('You are {{  bold }}',
                   '{{#_}}You are {{  bold }}{{/_}}')
        self.check('You are {{{bold}}}',
                   '{{#_}}You are {{{bold}}}{{/_}}')

    def test_handlebars_comment(self):
        self.check('You are {{! make bold }}bold',
                   '{{#_}}You are{{/_}} {{! make bold }}{{#_}}bold{{/_}}')

    def test_handlebars_block(self):
        self.check('You are {{#if bold}}bold{{else}}timid{{/if}}',
                   '{{#_}}You are{{/_}} {{#if bold}}{{#_}}bold{{/_}}'
                   '{{else}}{{#_}}timid{{/_}}{{/if}}')

    def test_handlebars_variable_in_tag(self):
        self.check('You are <b {{how_bold}}>bold</b>',
                   '{{#_}}You are <b {{how_bold}}>bold</b>{{/_}}')

        self.check('You are <b {{ how_bold    }}>bold</b>',
                   '{{#_}}You are <b {{ how_bold    }}>bold</b>{{/_}}')

        self.check('You are <b {{how_bold}} id=a>bold</b>',
                   '{{#_}}You are <b {{how_bold}} id=a>bold</b>{{/_}}')

        self.check('You are <b id=a {{how_bold}}>bold</b>',
                   '{{#_}}You are <b id=a {{how_bold}}>bold</b>{{/_}}')

        self.check('You are <b id=a {{how_bold}} name=b>bold</b>',
                   '{{#_}}You are <b id=a {{how_bold}} name=b>bold</b>{{/_}}')

    def test_escape_tag_attributes_with_single_quotes(self):
        self.check('<input value="john\'s input" type=button>',
                   '<input value="{{#_}}john\'s input{{/_}}" type=button>')

    def test_handlebars_conditional_attr_in_tag(self):
        self.check('You are <b {{#if bold}}very{{/if}}>bold</b>',
                   '{{#_}}You are <b {{#if bold}}very{{/if}}>'
                   'bold</b>{{/_}}')

        self.check('You are <b {{# if bold  }}very{{/  if }}>bold</b>',
                   '{{#_}}You are <b {{# if bold  }}very{{/  if }}>'
                   'bold</b>{{/_}}')

        self.check('You are <b foo=1 {{#if bold}}very{{/if}} baz=2>bold</b>',
                   '{{#_}}You are <b foo=1 {{#if bold}}very{{/if}} baz=2>'
                   'bold</b>{{/_}}')

        self.check('You are <b foo=1{{#if bold}} very{{/if}} baz=2>bold</b>',
                   '{{#_}}You are <b foo=1{{#if bold}} very{{/if}} baz=2>'
                   'bold</b>{{/_}}')

        # TODO(csilvers): this gives a parse error now.
        self.todo('You are <b foo=1 {{#if bold}}very {{/if}}baz=2>bold</b>',
                  '{{#_}}You are <b foo=1 {{#if bold}}very {{/if}}baz=2>'
                  'bold</b>{{/_}}')

        self.check('You are <b {{#if bold}}very{{else}}not{{/if}}>bold</b>',
                   '{{#_}}You are <b {{#if bold}}very{{else}}'
                   'not{{/if}}>bold</b>{{/_}}')

        self.check('You are <b{{#if bold}} very{{else}} not{{/if}}>bold</b>',
                   '{{#_}}You are <b{{#if bold}} very{{else}}'
                   ' not{{/if}}>bold</b>{{/_}}')

    def test_handlebars_nested_conditional_attr_in_tag(self):
        # The main check here is that we don't get a 'malformed tag' error.
        self.check('You are <b {{#if bold}}{{#_}}very{{/_}}{{/if}}>bold</b>',
                   '{{#_}}You are <b {{#if bold}}{{#_}}very{{/_}}{{/if}}>'
                   'bold</b>{{/_}}')

    def test_handlebars_containing_tags(self):
        self.check('<a href="foo">hello</a>',
                   '<a href="foo">{{#_}}hello{{/_}}</a>')

    def test_handle_leading_trailing_nonpunct(self):
        self.check('   hello  ', '   {{#_}}hello{{/_}}  ')

        self.check('<a href="foo">   hello  </a>',
                   '<a href="foo">   {{#_}}hello{{/_}}  </a>')

    def test_handle_some_containing_tags(self):
        self.check('<b><a href="foo">hello</a> there</b>',
                   '<b>{{#_}}<a href="foo">hello</a> there{{/_}}</b>')

    def test_no_double_escaping(self):
        self.check('<p>{{#_}}Hello, world{{/_}}</p>',
                   '<p>{{#_}}Hello, world{{/_}}</p>')

        self.check('<p>{{#ngettext}}Hello, 1 world'
                   '{{else}}Hello, %(num)d worlds{{/ngettext}}</p>',
                   '<p>{{#ngettext}}Hello, 1 world'
                   '{{else}}Hello, %(num)d worlds{{/ngettext}}</p>')

    def test_do_not_translate_function(self):
        self.check('{{#i18nDoNotTranslate}}same in all'
                   'languages{{/i18nDoNotTranslate}}',
                   '{{#i18nDoNotTranslate}}same in all'
                   'languages{{/i18nDoNotTranslate}}')


class CustomizationTest(unittest.TestCase):
    def setUp(self):
        self.old_NLTA = i18nize_templates.NATURAL_LANGUAGE_TAG_ATTRIBUTES
        i18nize_templates.NATURAL_LANGUAGE_TAG_ATTRIBUTES = (
            i18nize_templates.NATURAL_LANGUAGE_TAG_ATTRIBUTES[:])

        i18nize_templates._NL_ATTR_MAP = None
        
        self.old_NLSCRE = (
            i18nize_templates.NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE_STRING)
        i18nize_templates.NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE_STRING = (
            i18nize_templates.NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE_STRING[:])

        self.old_NLSCR = (
            i18nize_templates._NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE)

        self.old_OF = i18nize_templates._OK_FUNCTIONS
        i18nize_templates._OK_FUNCTIONS = i18nize_templates._OK_FUNCTIONS[:]

        text_handler = i18nize_templates.Jinja2TextHandler
        tag_parser = i18nize_templates.Jinja2HtmlLexer(
            text_handler(None).handle_segment)
        self.parser = i18nize_templates.Jinja2HtmlLexer(
            text_handler(tag_parser).handle_segment)

    def tearDown(self):
        i18nize_templates.NATURAL_LANGUAGE_TAG_ATTRIBUTES = self.old_NLTA
        i18nize_templates._NL_ATTR_MAP = None
        i18nize_templates.NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE_STRING = (
            self.old_NLSCRE)
        i18nize_templates._NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE = (
            self.old_NLSCR)
        i18nize_templates._OK_FUNCTIONS = self.old_OF

    def check(self, input, expected):
        # We check twice: once with the global parser, which was set
        # up before the customization routines were called, and once
        # with a local parser, which was set up after the
        # customization routines were called.  We should get the same
        # answer each time.
        actual = self.parser.parse(input)
        self.assertEqual(expected, actual)

        text_handler = i18nize_templates.Jinja2TextHandler
        tag_parser = i18nize_templates.Jinja2HtmlLexer(
            text_handler(None).handle_segment)
        parser = i18nize_templates.Jinja2HtmlLexer(
            text_handler(tag_parser).handle_segment)
        actual = parser.parse(input)
        self.assertEqual(expected, actual)

    def todo(self, input, expected):
        pass

    def test_add_nltext_tag_attribute(self):
        self.check('foo<p val="hello" why="me">',
                   '{{ _("foo") }}<p val="hello" why="me">')

        i18nize_templates.add_nltext_tag_attribute('p', 'val', ('why', 'me'))

        self.check('foo<p val="hello" why="me">',
                   '{{ _("foo") }}<p val="{{ _("hello") }}" why="me">')
        self.check('foo<p val="hello" why="not me">',
                   '{{ _("foo") }}<p val="hello" why="not me">')
        self.check('foo<div val="hello" why="me">',
                   '{{ _("foo") }}<div val="hello" why="me">')
        
    def test_add_nltext_separator_class(self):
        self.check('foo<br class="sepb">bar',
                   r'{{ _("foo<br class=\"sepb\">bar") }}')

        i18nize_templates.add_nltext_separator_class('sep[a-e]')

        self.check('foo<br class="hi sepb lo">bar',
                   '{{ _("foo") }}<br class="hi sepb lo">{{ _("bar") }}')
        self.check('foo<br class="sepz">bar',
                   r'{{ _("foo<br class=\"sepz\">bar") }}')

    def test_mark_function_args_lack_nltext(self):
        self.check('{{ myfn("arg") }}', '_TODO({{ myfn("arg") }})')

        i18nize_templates.mark_function_args_lack_nltext("myfn")
        
        self.check('{{ myfn("arg") }}', '{{ myfn("arg") }}')
        self.check('{{ foo_myfn("arg") }}', '_TODO({{ foo_myfn("arg") }})')
        # TODO(csilvers): this should pass too: \b isn't good enough.
        self.todo('{{ foo.myfn("arg") }}', '_TODO({{ foo.myfn("arg") }})')


if __name__ == '__main__':
    unittest.main()
