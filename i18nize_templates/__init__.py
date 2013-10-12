#!/usr/bin/env python

"""A tool to add {{ _("...") }} around text in template files.

This works for jinja2 template files (and presumably django template
files) and handlebars template files.

This is part of a process to make a non-i18n-aware jinja2 file
i18n-aware.  i18n-ness support is mostly a matter of marking
natural-language text in the file that needs to be translated.  While
some complicated natural language constructs (like plurals) require a
bit more work, the simple case is very simple: replace
   <p>Hello <b>world</b></p>
with
   <p>{{ _("Hello <b>world</b>") }}</p>

This script does that, or at least attempts to -- it makes not
pretentions toward perfection.  For situations where it's pretty sure
there's natural language text but it doesn't know how to handle it, it
will insert text like this:
   <p>{{ _TODO("Hello {{world}}") }}</p>

This will require manual cleanup before the template file is usable.

Note that this is just a tool, and will not capture all
natural-language text.  For instance, a manual pass will still be
needed to detect natural-language text inside
(1) javascript <script> tags
(2) jinja2 {% set %} directives
(3) custom tag attributes (e.g. if you have <span alert="msg">)
(4) ...

On the other hand, the tool may mark up some text that isn't actually
natural language:
   <input type="button" value="x">
You can mark this text up like so to avoid i18nize_templates touching it:
   <input type="button" value="{{ i18n_do_not_translate("x") }}">

Note that this script only uses the gettext-style _() format for
translating text; it never uses {% trans %} or {% blocktrans %}.


-- CUSTOMIZING:

*** add_nltext_tag_attribute("span", "myapp-description")

If you have your own application-specific html tag attributes that
hold natural language text, you can tell this script about them via
this function call.  For example, i18nize_templates will know that
'xxx' is natural langauge text in <span myapp-description="xxx">.

*** add_nltext_separator_class("separator")

Likewise, if you have application-specific classes that separate
natural language text, you can specify them via
add_nltext_separator_class().  Note this function takes a regular
expression string, not a literal text string.

This means that 'a<br class="highlight separator">b' will be
treated as two different nltext runs, even though normally a br tag
would not separate runs.

*** mark_function_args_lack_nltext("log.date.strftime")

This is used for function calls, or filters, made from within django
or jinja2 templates.  This script assumes that these function calls
have natural-language text as arguments, so when it sees literal
strings for these arguments, it marks them for translation.  If you
know that the function takes only arguments that do not need to be
translated, you can call mark_function_args_lack_nltext() on it.


-- HOW IT WORKS:

This problem would usually be solved by an html parser, but that
doesn't work well for jinja2 files, which may not be valid html due to
the jinja markup.  (An actual example I just happened upon:
   <img title={% if something %}"yes"{% else %}"no"{% endif %} ...>
.  Some html parsers -- not HTMLParser, sadly -- could probably muddle
through this, but something similar would likely trip them up.)

We use a munged form of HTMLParser, that has been modified to treat
jinja2 markup as a single character.

--- THE LEXER

This file has two main classes.

The first, the lexer, deals with splitting up the text into
'segments'.  Each segment is a single entity: an html tag, a run of
text, etc.  For the base HTMLLexer, the only segments are tags
(including start tags, end tags, comment tags, doctype tags, PI tags,
etc) and not-tags.  For the jinja2 lexer, the segments are html tags,
jinja2 {{variables}}, jinja2 {#comments#}, jinja2 {%block constructs
%}, and everything-else.  And so forth.

Example: <body>Hi, there!<script>foo<x && y>bar</script><b>Weird</b>

Has the following segments
   '<body>', 'Hi, there!', '<script>', 'foo<x && y>bar',
   '</script>', '<b>', 'Weird', '</b>'.
(Note the contents of the script-tag are a single segment; the lexer
is smart enough to handle CDATA inside <script> and <style> as a
single entity.)

Besides breaking up text into segments, the lexer marks each segment
as being "part of a run of natural language text" or "breaking up a
run of natural language text".  A 'run' of natural language text is
what one would intuitively think of as a single 'block' of text.  It's
the unit of translation: translators translate one run of text at a
time.

This is best illustrated with an example:
   <p>Hi, <b>you</b>.</p><p>How are you doing?</p>
The segments are '<p>', 'Hi, ', '<b>', 'you', '</b>', '.', '</p>',
'<p>', 'How are you doing?, '</p>'.  In this, there are two runs of
natural-language text: 1) 'Hi, <b>you</b>.' and 2) 'How are you
doing?'.  Note that the second run of natural-language text is a
single segment, but the first run is 5 segments.  And a few segments
that the lexer found ('<p>', </p>') aren't included in either run.

The lexer knows which kinds of segments contribute to runs of
natural-language text, and which segments, when we see them, cause a
new run to start.  The former consists of 'inline' html tags ('<b>',
'<img>', etc), and things that aren't html tags.  The latter consists
of everything else.  The jinja2 parser has its own mapping of
segment-types: jinja2 {%blocks%} break up nl text, while jinja2
{{variables}} do not.  (So, this is one nl run: 'Hi there, {{name}}!'.)

When the lexer sees a segment, it calls into a function that is
provided to it in its constructor, passing in the contents of the
segment, and also whether or not this segment contributes to an
nl-text run, or breaks one up.

--- THE TEXT HANDLER

This is the function called by the lexer when it sees a segment.  Its
job is to join the runs of natural language text into a single string,
which it can then put _("...") around.  (For handlebars templates, it
puts {{#_}}...{{/#}} around instead.)  Continuing our example, the
lexer would make the following callbacks.
   some_text_handler('<p>',  breaks_nltext=True)
   some_text_handler('Hi, ', breaks_nltext=False)
   some_text_handler('<b>',  breaks_nltext=False)
   some_text_handler('you',  breaks_nltext=False)
   some_text_handler('</b>', breaks_nltext=False)
   some_text_handler('.',    breaks_nltext=False)
   some_text_handler('</p>', breaks_nltext=True)
   some_text_handler('<p>',  breaks_nltext=True)
   some_text_handler('How are you doing?', breaks_nltext=False)
   some_text_handler('</p>', breaks_nltext=True)
   some_text_handler(None, breaks_nltext=True)      # end-of-document

The text handler should emit:
   <p>{{ _("Hi, <b>you</b>.") }}</p><p>{{ _("How are you doing?") }}</p>

Its algorithm is pretty simple: when it sees a segment with
breaks_nltext=False, it collects it up.  Whenever it sees a segment
with breaks_nltext=True, it concatenates together the collected-up
segments, puts '{{ _("...") }}' around it, and emits it.  Then it also
emits the breaks-nltext text (stuff that breaks up natural-language
runs is never marked up, and can always be emitted verbatim).

This work is made (much) more complicated by various optimizations we
put in to make life simpler for translators.  For instance, for html
like '<p>hi</p>\n', the newline is its own nl-text segment, but we
don't want to emit '{{ _("\n") }}' -- translators don't need to
translate the newline character!  Likewise, if the text is
'<b>&lt; Hi &gt;</b>', it's best to emit '<b>&lt; {{ _("Hi") }} &gt;</b>',
rather than '{{ _("<b>&lt; Hi &gt;</b>") }} -- there's no need to force
the translators to copy over the bold tags and the punctuation.  So
there are regexps and rather complex logic to identify where 'actual
natural language text' starts and ends within a natural-language run.


USAGE: $0 < infile > outfile
   OR  $0 <file> ...

The second usage modifies the files in-place.
"""

import HTMLParser      # for HTMLParseError
import argparse
import htmlentitydefs
import markupbase
import re
import string
import sys
import unicodedata


# Taken from http://www.htmlhelp.com/reference/html40/inline.html
# I've removed 'input' and 'textarea' since, while inline, they still
# divide up natural-language text.
INLINE_ELEMENTS = frozenset((
        'a', 'abbr', 'acronym', 'b', 'basefont', 'bdo', 'big', 'br',
        'cite', 'code', 'dfn', 'em', 'font', 'i', 'img', 'kbd',
        'label', 'q', 's', 'samp', 'select', 'small', 'span', 'strike',
        'strong', 'sub', 'sup', 'tt', 'u', 'var',
        ))


# These are tag-attributes that have natural language text in them.
# The format is: (tagname, attribute, (prereq-key, prereq-value)) or
# just (tagname, attribute).  We interpret this as: attribute
# ATTRIBUTE in tag TAGNAME has natural language text whenever
# prereq-key is also in the tag, and has value prereq-value.
# TODO(csilves): move placeholder and data-desc to ka-specific code.
NATURAL_LANGUAGE_TAG_ATTRIBUTES = [
    ('*', 'title'),
    ('*', 'placeholder'),      # usually seen on input and textarea
    ('*', 'data-desc'),        # usually seen on p and span
    ('img', 'alt'),
    ('input', 'value', ('type', 'button')),
    ('input', 'value', ('type', 'submit')),
    ('input', 'value', ('type', 'text')),
]

# A more code-friendly version of the above, set lazily.
_NL_ATTR_MAP = None


# These are tag-classes that indicate an element separates
# natural-language text, even though it might seem not to.  For
# instance (<span class="separator">) is presumably not part of a
# natural-language sentence, but instead is separating them.
# TODO(csilves): move 'button' and 'btn' to ka-specific code.
NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE_STRING = [
    '(.*-)?button(-.*)?',
    '(.*-)?btn(-.*)?',
    'separator',
]

_NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE = re.compile(
    '(^| )(%s)( |$)' % '|'.join(NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE_STRING))


_OK_FUNCTIONS = [
    # jinja2 functions/filters
    'attr',
    'dictsort',
    'groupby',
    'int',
    'map',
    'reject',
    'rejectattr',
    'select',
    'selectattr',
    'sort',
    'striptags',
    'sum',
    'urlize',
    # django functions/filters
    'center',
    'date',
    'dictsortreversed',
    'divisibleby',
    'floatformat',
    'get_digit',
    'length_is',
    'ljust',
    'removetags',
    'rjust',
    'slice',
    'stringformat',
    'time',
    # Khan-academy specific.  TODO(csilvers): move to KA-specific place.
    'js_css_packages.package',
    'handlebars_template',
    'youtube.player_embed',
    'log.date.strftime',
    ]
    

# --- customization API calls

def add_nltext_tag_attribute(tagname, attribute_name, prereqs=None):
    """Globally register a particular tag attribute as having nl text.

    Some tag attributes, such as <img alt="xxx">, contain natural
    language text.  While i18nize_templates hard-codes in the standard
    tags, if your application uses application-specific tag
    attributes, you can add them here.  For instance, if you have
       <span myapp-description="help is coming!">
    you could call add_nltext_tag_attribute("span", "myapp-description").

    Arguments:
       tagname: the tag to match, or '*' to match all tags.
       attribute_name: the attribute to match.  We mark this
          attribute as having natural language text whenever
          it's seen in tag 'tagname' (if 'tagname' is '*', it's
          said to hold nltext no matter where it's seen) --
          subject to the prereqs.
       prereqs: if not None, a pair of attribute name and value.
          When specified, we say the tag/attribute_name pair only
          holds natural langauge text when the prereq attr-val
          is also present.  For instance:
              add_nltext_tag_attribute('input', 'value', ('type', 'button'))
          says that <input value="xxx" type=button> holds a natural
          language value, but <input value="xxx" type=submit> does not.
    """
    global _NL_ATTR_MAP
    if prereqs:
        entry = (tagname, attribute_name, prereqs)
    else:
        entry = (tagname, attribute_name)
    NATURAL_LANGUAGE_TAG_ATTRIBUTES.append(entry)
    # Force this to be re-computed on the next call.
    _NL_ATTR_MAP = None


def add_nltext_separator_class(classname_re_string):
    """Add a tag-class that indicates an element separates nltext.

    Usually tags like <span> and <br> are considered part of a natural
    language sentence, rather than separating natural language into
    distinct runs.  However in some situations that might more
    accurately serve as separators.  If those situations are denoted
    by using a particular class, then you can indicate that by calling
    this routine.  For instance:
       add_nltext_separator_class("(.*-)?btn(-.*)?")
    means that 'a<span class="warning big-btn"></span>b' will be
    parsed as two distinct nltext runs ('a' and 'b') rather than one.

    Arguments:
       classname_re_string: a regular expression string that indicates
          what classes should be considered nltext separators.  We
          match if this class exists as a separator word in a tag's
          'class' tag.
    """
    global _NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE
    NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE_STRING.append(classname_re_string)
    _NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE = re.compile(
        '(^| )(%s)( |$)' % '|'.join(
            NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE_STRING))


def mark_function_args_lack_nltext(fn_name):
    """Indicate that a given function has non-nltext string arguments.

    For jinja2 (and possibly also for django), you can call built-in
    and user-defined functions from within the template:
       {{ my_function("arg1", "arg2") }}
    When it sees string literals as arguments, as in arg1 and arg2
    above, i18nize_templates has to assume that these strings are
    natural-language text, and it marks them for translation.  If
    they are not natural language text, you can use
       {{ my_function(i18n_do_not_translate("arg1"), ...) }}    
    but easier is to call mark_function_args_lack_nltext("my_function").

    Arguments:
       fn_name: the name of the function to add to the whitelist.
    """
    _OK_FUNCTIONS.append(fn_name)
    # We need to recalculate the users of _OK_FUNCTIONS.
    # TODO(csilvers): make less fragile.
    Jinja2TextHandler.J2_VAR_NO_NATURAL_LANGUAGE = re.compile(
        Jinja2TextHandler.J2_VAR_NO_NATURAL_LANGUAGE_STRING
        % '|'.join(_OK_FUNCTIONS))

# --- end customization API calls


def _case_insensitive_hex(s):
    """Replace a-e with [aA]-[fF]."""
    for (frm, to) in (('a', '[aA]'), ('b', '[bB]'), ('c', '[cC]'),
                      ('d', '[dD]'), ('e', '[eE]'), ('f', '[fF]')):
        s = s.replace(frm, to)
    return s

# Find the entities that are not alnums -- by name and by number --
# and put them all into a big regexp.  The unicode category 'L' means
# letter, and 'N' means number.  We have to add in apos manually,
# since it's not part of html4.  We add in 7-bit ascii as well.
_NON_ALNUM_ENTITIES = 'apos' + ''.join(
    '|%s|#%s|#[xX]%s' % (name, num, _case_insensitive_hex(hex(num)[2:]))
    for (name, num) in htmlentitydefs.name2codepoint.iteritems()
    if not unicodedata.category(unichr(num))[0] in ('L', 'N'))
_NON_ALNUM_ENTITIES += ''.join(
    '|#%s|#[xX]%s' % (ord(c), _case_insensitive_hex(hex(ord(c))[2:]))
    for c in (string.punctuation + string.whitespace))


def natural_language_attributes(tagname, attrval_pairs):
    """Yields indices into attrval_pairs where the value is natural language.

    For instance, if tagname is 'input' and attrval_pairs is
    [('type', 'button'), ('value', 'foo')], then this will yield [1].
    We yield the indices in reverse order, so we can rewrite one
    without affecting the positions of the next ones.
    """
    global _NL_ATTR_MAP
    if _NL_ATTR_MAP is None:
        # Convert this to a map:
        #    tagname -> {attr: ((pre-k1, pre-v1), ...)), ...}
        # This means 'attr has a natural-language value when found in
        # tagname, if *any* of (pre-k1, pre-v1), (pre-k2, pre-v2), etc
        # are also found in the tag.'  (pre-k1, pre-v1) can also be
        # None to mean 'no prereqs needed.'
        _NL_ATTR_MAP = {}
        for entry in NATURAL_LANGUAGE_TAG_ATTRIBUTES:
            _NL_ATTR_MAP.setdefault(entry[0], {})
            _NL_ATTR_MAP[entry[0]].setdefault(entry[1], [])
            if len(entry) == 3:
                _NL_ATTR_MAP[entry[0]][entry[1]].append(entry[2])
            else:
                _NL_ATTR_MAP[entry[0]][entry[1]].append(None)   # no prereq

    for i in xrange(len(attrval_pairs) - 1, -1, -1):
        (attr, val) = attrval_pairs[i]
        # If there's no value, we definitely don't need to munge it...
        if val is None:
            continue

        # Sometimes an attribute-value only has natural-language
        # text in it when another attribute is also present.
        # Figure out what those pre-reqs are: they will be in
        # _NL_ATTR_MAP[tagname][attr] or _NL_ATTR_MAP['*'][attr].
        # If they're absent from both, then this tagname/attr
        # pair cannot have a natural-language value.
        prereqs = (_NL_ATTR_MAP.get(tagname, {}).get(attr) or
                   _NL_ATTR_MAP.get('*', {}).get(attr) or [])
        for prereq in prereqs:
            if prereq is None or prereq in attrval_pairs:
                # OK, this attr has natural language text, and all the
                # pre-requisites for i18nizing it are satisified.  Go!
                yield i
                break


class HtmlLexer(markupbase.ParserBase):
    """Find tags and other markup and call handler functions.

    Usage:
        p = HtmlLexer()
        p.parse(data)

    This is similar to the standard library HTMLParser, but simpler
    in that it doesn't try to handle entities, and structured to
    make it easy to extend the parser via subclassing.

    The constructor takes a function object as an argument:
        callback(segment, segment_separates_nltext)

    The callback takes a segment of the input document, and a boolean
    saying whether this segment can be part of a natural-language run,
    or breaks up natural language runs (see top-of-file docstring for
    more info about this, but tags such as '<b>' and all non-tags fall
    into the first category, and tags such as '<p>' fall into the
    second).

    At the end of parsing, callback() is called one more time with
    segment == None.  This allows it to do post-processing if needed.

    The callback should return in one of three different ways:
       1) Return a string (possibly the empty string) on every call.
          In this case, HtmlLexer.parse() will concatenate all these
          strings together, and parse() will return that result.
       2) Return None on every call but the last (when segment == None).
          On the last call, it can return an arbitrary object, which
          parse() will return in turn.
       3) Never return anything.  In this case, parse() will return
          None.  (This is a special case of (2).)
    """
    # 'Interesting' means: when does the next entity start?  For the
    # html parser, only tags are interesting entities.  (In CDATA
    # mode, most tags are interpreted literally, and only the
    # </script> or </style> tag is 'interesting'.)
    INTERESTING_NORMAL = re.compile('<')
    INTERESTING_CDATA = lambda cls, tag: re.compile(r'</\s*%s' % tag)

    TAGFIND = re.compile('[a-zA-Z][-.a-zA-Z0-9:_]*(?:\s+|(?=>|/>))')
    ENDTAGFIND = re.compile('</\s*([a-zA-Z][-.a-zA-Z0-9:_]*)\s*>')
    PICLOSE = '>'

    _ATTR_NAME = r'[a-zA-Z_][-.:a-zA-Z0-9_]*'

    # just 'attr', not 'attr=value'.
    BARE_ATTR = re.compile(r'(?P<attr>%s)(?:\s+(?!=)|(?=>|/>))' % _ATTR_NAME,
                           re.DOTALL)

    ATTR_AND_VALUE = re.compile(
        r'(?P<attr>%(attr)s)\s*=\s*'
        r"(?:'(?P<lita>(?:[^'])*)'"    # LITA
        r'|"(?P<lit>(?:[^"])*)"'       # LIT
        r'|(?P<bare>(?:[^\'\">\s])+)'  # bare value
        r')(?:\s+|(?=>|/>))'   # end
        % {'attr': _ATTR_NAME}, re.DOTALL)

    def __init__(self, callback):
        markupbase.ParserBase.__init__(self)
        # markupbase makes some callbacks we don't care about.
        for fn in ('handle_comment', 'handle_decl', 'unknown_decl'):
            setattr(self, fn, lambda *args, **kwargs: None)

        self.callback = callback
        # initialize line number and position
        self.reset()

    def _call_callback(self, segment, segment_separates_nltext):
        """Call the callback and update self.callback_outputs."""
        # We just accumulate all the non-None callback-outputs, and
        # let HtmlLexer.parse() figure out which of the 3 cases
        # (listed in HtmlLexer.__doc__) we fall under.
        retval = self.callback(segment, segment_separates_nltext)
        if retval is not None:
            self.callback_outputs.append(retval)

    def error(self, message):
        raise HTMLParser.HTMLParseError(message, self.getpos())

    def set_cdata_mode(self, tag):
        self.interesting = self.INTERESTING_CDATA(tag)

    def clear_cdata_mode(self):
        self.interesting = self.INTERESTING_NORMAL

    def is_cdata_tag(self, tagname, attrs):
        if tagname == 'style':
            return True

        if tagname == 'script':
            # True for 'normal' script tags, but some people sneakily
            # abuse the script tag to store html content:
            # cf. http://ejohn.org/blog/javascript-micro-templating/
            # They tend to use type=text/html, so we look for that.
            for (attr, value) in attrs:
                if attr == 'type' and 'text/html' in value:
                    return False
            return True

        return False

    def handle_data(self, rawdata, start, end):
        """Handles everything between the html tags.  Excludes script data."""
        # By definition, 'data' is natural language text: this is
        # called for everything that's not meaningful html.
        if self.interesting == self.INTERESTING_NORMAL:
            self._call_callback(rawdata[start:end],
                                segment_separates_nltext=False)
        else:
            # Inside a script-tag, there's no such thing as nltext.
            self._call_callback(rawdata[start:end],
                                segment_separates_nltext=True)

    def handle_tag(self, rawdata, start, end, tagname, attrs):
        """Handles a start tag or an end tag.  Excludes comments, PI, etc."""
        segment_separates_nltext = tagname not in INLINE_ELEMENTS

        # As a special case, we treat '<br><br>' as a text separator,
        # since people often use it as a substitute for '<p>'.  We
        # *could* look for <br>\n<BR>, etc. but there's no need yet.
        if (not segment_separates_nltext and
            rawdata.startswith('<br><br>', start) or
            rawdata.startswith('<br/><br/>', start)):
            segment_separates_nltext = True

        # Another special case: certain classes (defined at the top
        # of the file) indicate a tag is separating natural language.
        if not segment_separates_nltext:
            for (attr, val) in attrs:
                if (attr == 'class' and
                    _NATURAL_LANGUAGE_SEPARATOR_CLASSES_RE.search(val)):
                    segment_separates_nltext = True
                    break

        self._call_callback(rawdata[start:end], segment_separates_nltext)

    def parse_to(self, to, i):
        """Find the next occurrence of 'to', past i, and return pos past it."""
        end = self.rawdata.find(to, i)
        if end == -1:
            self.error("No end %s found" % to)
        return end + len(to)

    def parse_starttag(self, rawdata, i):
        """Given the beginning of a start tag, return its end location."""
        tagstart = i

        # First, find the tagname.
        match = self.TAGFIND.match(rawdata, i + 1)
        assert match, 'unexpected call to parse_starttag(): pos %s' % i
        tag = rawdata[i + 1:match.end()].lower().strip()
        i = match.end()

        attrs = []
        value_poses = []        # (start, end), from position of opening <
        while not (rawdata.startswith('>', i) or rawdata.startswith('/>', i)):
            # Check if we have a complete attr-value pair: an attribute
            # name that doesn't end with an =.
            match = self.BARE_ATTR.match(rawdata, i)
            if match:
                attrs.append((match.group('attr'), None))
                value_poses.append((None, None))
                i = match.end()
                continue

            match = self.ATTR_AND_VALUE.match(rawdata, i)
            if match:
                attr = match.group('attr')
                # one of the LITA/LIT/<bare> groups contains the
                # value, the others contain None (it depends on the
                # value format).
                for n in ('lita', 'lit', 'bare'):
                    if match.group(n) is not None:
                        attrs.append((attr, match.group(n)))
                        value_poses.append((match.start(n) - tagstart,
                                            match.end(n) - tagstart))
                        break
                else:     # for/else: no value found  ('<tag attr=>')
                    attrs.append((attr, ''))
                    value_poses.append((0, 0))
                i = match.end()
                continue

            self.error('Malformed "%s" tag' % tag)

        if rawdata.startswith('>', i):
            if self.is_cdata_tag(tag, attrs):
                self.set_cdata_mode(tag)
            return (i + 1, tag, attrs, value_poses)
        elif rawdata.startswith('/>', i):
            return (i + 2, tag, attrs, value_poses)
        raise AssertionError("we should not get here!")

    def parse_endtag(self, rawdata, i):
        """Given the beginning of an end tag, return its end location."""
        match = self.ENDTAGFIND.match(rawdata, i)  # </ + tag + >
        if not match:
            self.error("Incomplete or bad end tag")
        tag = match.group(1).strip()

        self.clear_cdata_mode()
        return (match.end(), tag)

    def parse_entity(self, i):
        """Parse the 'interesting' entity at self.rawdata[i].

        This is called for every place in self.rawdata that matches
        self.interesting.  We should parse the entity that exists
        at this location, call the proper callbacks for it, call
        self.updatepos(i, end_of_entity), and return end_of_entity.

        Arguments:
            i: an offset into self.rawdata.

        Returns:
            An offset into self.rawdata pointing to the end of the
            entity at position i, or None if there was no
            'interesting' entity at position i after all.
        """
        rawdata = self.rawdata
        if rawdata.startswith('<', i):
            if self.TAGFIND.match(rawdata, i + 1):
                (entity_end, tagname, attrs, _) = self.parse_starttag(rawdata,
                                                                      i)
                self.handle_tag(rawdata, i, entity_end, tagname, attrs)
            elif rawdata.startswith("</", i):
                (entity_end, tagname) = self.parse_endtag(rawdata, i)
                self.handle_tag(rawdata, i, entity_end, tagname, [])
            elif rawdata.startswith("<!--", i):
                entity_end = self.parse_comment(i)      # in markupbase
                # We *could* collect text like 'a<!-- b -->c', but...
                self._call_callback(rawdata[i:entity_end],
                                    segment_separates_nltext=True)
            elif rawdata.startswith("<?", i):
                entity_end = self.parse_to(self.PICLOSE, i)
                self._call_callback(rawdata[i:entity_end],
                                    segment_separates_nltext=True)
            elif rawdata.startswith("<!", i):
                entity_end = self.parse_declaration(i)  # in markupbase
                self._call_callback(rawdata[i:entity_end],
                                    segment_separates_nltext=True)
            else:
                entity_end = i + 1
                self.handle_data(rawdata, i, entity_end)
            return self.updatepos(i, entity_end)

        return None

    def parse(self, rawdata):
        """Lex the contents of rawdata, calling a callback for each segment.

        Arguments:
           rawdata: the document to parse.

        Returns:
           Forwards the return value of the callback as described in
           HtmlLexer.__doc__.
        """
        markupbase.ParserBase.reset(self)
        self.rawdata = rawdata
        self.callback_outputs = []
        self.interesting = self.INTERESTING_NORMAL

        i = 0
        n = len(rawdata)
        while i < n:
            # This parser works like a two-stroke engine, repeatedly
            # doing the following two steps:
            # 1) Parse everything up to the next html tag as a single
            #    'data' segment.  This is always a natural-language
            #    text segment.
            # 2) Parse the next html tag as a single 'data' segment.
            #    This may or may not be a natural-language segment,
            #    depending on the tag.
            match = self.interesting.search(rawdata, i)  # <
            if match:
                entity_start = match.start()
            else:
                entity_start = n

            # 1) Everything up to the next tag.
            if i < entity_start:   # handle text up until the next entity
                self.handle_data(rawdata, i, entity_start)
            i = self.updatepos(i, entity_start)

            # 2) The next tag.
            if i < n:
                i = self.parse_entity(i)
                assert i is not None, "interesting.search() lied"

        assert i == n, (i, n)
        if self.callback_outputs:
            # Case (1) from HtmlLexer.__doc__: If the callback has
            # been returning values, then treat them as strings, and
            # concatenate and return them.  We need to make our
            # promised 'cleanup' callback call first, though.
            self._call_callback(None, segment_separates_nltext=True)
            return ''.join(self.callback_outputs)
        else:
            # Case (2) (and (3)) from HtmlLexer.__doc__: if the
            # callback has returned None up until now, we should
            # just forward the value of the 'cleanup' callback call.
            return self.callback(None, segment_separates_nltext=True)


class Jinja2HtmlLexer(HtmlLexer):
    """A version of HtmlLexer that can handle jinja2 markup.

    This is just like the html lexer, except it handles jinja2
    entities in a similar way to html tags.  Like html tags,
    jinja2 entities -- variables, blocks, comments -- either
    divide up runs of natural language, or can be included in a
    run of natural language.  Blocks and comments divide up runs,
    while jinja2 variables are included in runs.

    The only tricky bit is parsing tag names and attributes: jinja2
    elements can be inside html tags, gumming up the works.  So
    BARE_ATTR and ATTR_AND_VALUE are a little complicated.

    We need a specialized parser for this because jinja2 markup is
    processed *first*, before html markup, by the jinja2 template
    engine.  So we need a parser that treats it as a first-class
    element (and doesn't think {# <a href...> #} is a tag).
    """
    INTERESTING_NORMAL = re.compile('<|{{|{#|{%')
    # TODO(csilvers): make sure we ignore end </'s in {# jinja2 comments #}
    INTERESTING_CDATA = lambda cls, tag: re.compile(r'</\s*%s' % tag)

    # Matches {{[not }}]*}}
    _J2_VAR = r'{{(?:}?[^}])*}}'
    # Matches {% foo %}...{% else %}...{% etc %}...{% endfoo %}
    # Also, {%- foo -%}...{%- else -%}...{%- etc -%}...{%- endfoo -%}
    _J2_BALANCED_BLOCK = r'{%-?\s*(?!end).*?{%-?\s*end[^%]*%}'

    # Allow '<div{%if foo%} clear{%endif%}>'.
    TAGFIND = re.compile('[a-zA-Z][-.a-zA-Z0-9:_]*(?:\s+|(?=>|/>|%s))'
                         % _J2_BALANCED_BLOCK)

    # A 'bare' attribute (without a value) can look like an attribute-
    # name, or it can look like a jinja2 variable or a jinja2 block,
    # but either way it's followed by a space or tag-end, and not by an
    # equals.  (If it's a block, it could be an attribute-value pair;
    # it's too annoying for us to figure it out.)
    # NOTE: an attribute can end with a space, or with the tagend marker
    # (> or />).  In jinja2-land, it can also end with a jinja2 block:
    #    <a id="foo"{% if is_profile_empty %} class="empty"{% endif %}>
    BARE_ATTR = re.compile(
        r'(?P<attr>%s|%s|%s)(?:\s+(?!=)|(?=>|/>|%s))'
        % (HtmlLexer._ATTR_NAME, _J2_VAR, _J2_BALANCED_BLOCK,
           _J2_BALANCED_BLOCK),
        re.DOTALL)

    # Otherwise, the attribute has a value.  In this case, we require the
    # attribute be named explicitly (no '{{attr}}=1'), but the value can
    # be a raw string, a "quoted string" or 'quoted string', a {{jinja2
    # variable}}, or a {%jinja2 block%}.
    ATTR_AND_VALUE = re.compile(
        r'(?P<attr>%(attr)s)\s*=\s*'
        r"(?:'(?P<lita>(?:%(j2v)s|%(j2b)s|[^'])*)'"    # LITA
        r'|"(?P<lit>(?:%(j2v)s|%(j2b)s|[^"])*)"'       # LIT
        r'|(?P<bare>(?:%(j2v)s|%(j2b)s|[^\'\">\s])+)'  # bare value
        r')(?:\s+|(?=>|/>|%(j2b)s))'           # end
        % {'attr': HtmlLexer._ATTR_NAME,
           'j2v': _J2_VAR, 'j2b': _J2_BALANCED_BLOCK},
        re.DOTALL)

    def parse_entity(self, i):
        # First, see if it's an html entity.
        retval = HtmlLexer.parse_entity(self, i)
        if retval is not None:
            return retval

        # OK, maybe it's a jinja2 entity.
        # 3rd value here is true for tags that break up natural language runs.
        tags = (('{#', '#}', True),
                ('{%', '%}', True),
                # We never try to merge already-marked-up i18n
                # text with its neighbors.
                ('{{ _(', '}}', True),
                ('{{ _TODO(', '}}', True),
                ('{{ i18n_do_not_translate(', '}}', True),
                ('{{ ngettext(', '}}', True),
                ('{{', '}}', False))

        rawdata = self.rawdata
        for (starttag, endtag, segment_separates_nltext) in tags:
            if rawdata.startswith(starttag, i):
                entity_end = self.parse_to(endtag, i)
                self._call_callback(rawdata[i:entity_end],
                                    segment_separates_nltext)
                return self.updatepos(i, entity_end)

        # This is probably impossible: if we get here, it means
        # self.INTERESTING matched something not present in `tags`.
        return None


class HandlebarsHtmlLexer(HtmlLexer):
    """A version of HtmlLexer that can handle handlebars markup.

    This is just like the html lexer, except it handles handlebars
    entities in a similar way to html tags.  Like html tags,
    handlebars entities -- variables, blocks, comments -- either
    divide up runs of natural language, or can be included in a
    run of natural language.  Blocks and comments divide up runs,
    while handlebars variables are included in runs.

    We need a specialized parser for this because handlebars markup is
    processed *first*, before html markup, by the handlebars template
    engine.  So we need a parser that treats it as a first-class
    element (and doesn't think {{! <a href...> }} is a tag).
    """
    INTERESTING_NORMAL = re.compile('<|{{')
    # TODO(csilvers): make sure we ignore end </'s in {{! comments }}
    INTERESTING_CDATA = lambda cls, tag: re.compile(r'</\s*%s' % tag)

    # Matches {{[not }}]*}}
    _HBARS_VAR = r'{{{?[^!#/>](?:}?[^}])*}?}}'
    # Matches {{#foo}}...{{else}}...{{etc}}...{{/foo}}
    # We need to define this multiple times because the ?P= requires
    # unique names.  We use _HBARS_BALANCED_BLOCK 4 times in ATTR_AND_VALUE,
    # and each of those times needs a different (?P<name>...).
    # Passing in different values for 'i' provides that.
    _HBARS_BALANCED_BLOCK = lambda i: (
        r'{{#\s*(?P<hbb%d>[^\s}]+).*?{{/\s*(?P=hbb%d)\s*}}' % (i, i))

    # Allow '<div{{#if foo}} clear{{/if}}>'.
    TAGFIND = re.compile('[a-zA-Z][-.a-zA-Z0-9:_]*(?:\s+|(?=>|/>|%s))'
                         % _HBARS_BALANCED_BLOCK(1))

    # These are similar to Jinja2HtmlLexer.BARE_ATTR.
    BARE_ATTR = re.compile(
        r'(?P<attr>%s|%s|%s)(?:\s+(?!=)|(?=>|/>|%s))'
        % (HtmlLexer._ATTR_NAME, _HBARS_VAR, _HBARS_BALANCED_BLOCK(1),
           _HBARS_BALANCED_BLOCK(2)),
        re.DOTALL)

    ATTR_AND_VALUE = re.compile(
        r'(?P<attr>%(attr)s)\s*=\s*'
        r"(?:'(?P<lita>(?:%(hbv)s|%(hbb1)s|[^'])*)'"    # LITA
        r'|"(?P<lit>(?:%(hbv)s|%(hbb2)s|[^"])*)"'       # LIT
        r'|(?P<bare>(?:%(hbv)s|%(hbb3)s|[^\'\">\s])+)'  # bare value
        r')(?:\s+|(?=>|/>|%(hbb4)s))'           # end
        % {'attr': HtmlLexer._ATTR_NAME, 'hbv': _HBARS_VAR,
           'hbb1': _HBARS_BALANCED_BLOCK(1), 'hbb2': _HBARS_BALANCED_BLOCK(2),
           'hbb3': _HBARS_BALANCED_BLOCK(3), 'hbb4': _HBARS_BALANCED_BLOCK(4)},
        re.DOTALL)

    def parse_entity(self, i):
        # First, see if it's an html entity.
        retval = HtmlLexer.parse_entity(self, i)
        if retval is not None:
            return retval

        # OK, maybe it's a handlebars entity.
        # 3rd value here is true for tags that break up natural language runs.
        tags = (('{{!', '}}', True),
                # We never try to i18nize already-marked-up text.
                ('{{#_}}', '{{/_}}', True),
                ('{{#ngettext', '{{/ngettext}}', True),
                ('{{#i18nDoNotTranslate', '{{/i18nDoNotTranslate}}', True),
                # TODO(csilvers): how to i18n-ize function args?  Not
                # critical since we don't seem to have any nl text here.
                ('{{#', '}}', True),
                ('{{else', '}}', True),
                ('{{/', '}}', True),
                ('{{>', '}}', True),
                # This must come before '{{', so it matches first.
                ('{{{', '}}}', False),
                ('{{', '}}', False))

        rawdata = self.rawdata
        for (starttag, endtag, segment_separates_nltext) in tags:
            if rawdata.startswith(starttag, i):
                entity_end = self.parse_to(endtag, i)
                self._call_callback(rawdata[i:entity_end],
                                    segment_separates_nltext)
                return self.updatepos(i, entity_end)

        # This is probably impossible: if we get here, it means
        # self.INTERESTING matched something not present in `tags`.
        return None


class NullTextHandler(object):
    """An object that collects html text segments and spits them out unchanged.

    This is used to test HtmlLexer.  It does quite a bit of processing
    of its input: storing each segment passed to it according to
    whether it breaks up a run of natural-language text or continues
    one, chopping off non-text at the beginning and end of each
    nl-text run, etc.  But then, it basically throws all that work
    away, taking each processed segment and just concatenating them
    together.

    However, a subclass could easily do something more interesting
    with each processed segment.

    NullTextHandler.handle_entity() is an appropriate function to pass
    in to HtmlLexer or its subclasses.
    """
    # Used to test if a run of natural language actually has any
    # *language* in it.  For instance, ' ' is a run of natural
    # language as far as HtmlLexer is concerned, but not an
    # interesting one as far as we're concerned...
    NO_NATURAL_LANGUAGE = re.compile(r'^([^\w&]|&(%s)\b)*$'
                                     % _NON_ALNUM_ENTITIES, re.UNICODE)

    # We also want to ignore most natural language (especially
    # whitespace) at the start of a run of text.
    NO_NATURAL_LANGUAGE_PREFIX = re.compile(r'^([^\w&]|&(%s)\b)+'
                                            % _NON_ALNUM_ENTITIES, re.UNICODE)

    # Likewise, we want to ignore whitespace/etc at the end of a run of
    # text.  But we want to keep periods and other closing punctuation.
    NO_NATURAL_LANGUAGE_SUFFIX = re.compile(r'(\s|[@#$^*|]|&(%s)\b)+$'
                                            % _NON_ALNUM_ENTITIES, re.UNICODE)

    def __init__(self, tag_parser):
        """tag_parser is used to i18nize nl-text inside tag attributes."""
        self.nltext_segments = []
        self.tag_parser = tag_parser

    def is_entity(self, segment):
        """Return true if segment is an <html tag>, not a string of text."""
        return segment.startswith('<')

    def no_natural_language(self, segment):
        """Return true if a text-segment s has no natural-language text in it.

        While s may have been deemed to be a 'natural language
        segment' by HtmlLexer, that doesn't mean it has any actual
        natural language text in it: it could be a run of whitespace
        or whatever.  This checks whether it's actually a segment with
        language in it.

        We say that html tags have no nl-text by default, and neither
        do text-segments with just punctuation or whitespace.  Subclasses
        can override this to add their own checks.
        """
        return (self.is_entity(segment) or
                self.NO_NATURAL_LANGUAGE.match(segment))

    def add_underscore(self, s):
        return s

    def inside_tag(self):
        """If we don't have someone else to parse tags, then we're doing it."""
        return self.tag_parser is None

    def _segment_is_tag(self, segment, tagname):
        return (segment == '<%s>' % tagname or
                segment.startswith('<%s ' % tagname))

    def replace_natural_language_in_attributes(self, tag_contents, tagname,
                                               attrs, value_poses, munge_fn):
        """Replaces any nl attrs with the result of the munge_fn within tag."""

        # This yields the attrs backwards, so when we rewrite one,
        # it doesn't affect the positions of the next ones.
        for i in natural_language_attributes(tagname, attrs):
            (_, val) = attrs[i]
            munged_val = munge_fn(val)
            value_pos = value_poses[i]     # (start, end) pos of value
            tag_contents = (tag_contents[:value_pos[0]] + munged_val +
                            tag_contents[value_pos[1]:])

        return tag_contents

    def handle_tag(self, tag_contents):
        """Return a version of tag_contents with nl-text in attrs marked up."""

        if not self.tag_parser:
            return tag_contents

        # First, we call into the parser to get the tagname and attributes.
        try:
            (tag_end, tagname, attrs, value_poses) = (
                self.tag_parser.parse_starttag(tag_contents, 0))
            assert tag_end == len(tag_contents)
        except AssertionError:        # not actually a tag
            return tag_contents

        # Check whether it's possible this tag has natural-language attr-vals.
        if not attrs:
            return tag_contents

        # i18nize any nl attrs within the tag.
        tag_contents = self.replace_natural_language_in_attributes(
            tag_contents, tagname, attrs, value_poses, self.tag_parser.parse)

        return tag_contents

    def add_i18n(self, text_segments):
        """Adds _() and other appropriate i18n markup to text_segments.

        At its simplest, this routine could be just:
           return self.add_underscore(''.join(text_segments))

        But we complexify this routine to handle leading and trailing
        non-language elements.  For instance, if text_segments is
        ['   hi   '], we want to do
           return '   ' + self.add_underscore('hi') + '   '
        so the translators don't have to translate whitespace.
        Likewise, if text_segments is ['<b>', 'hi', '</b>'].

        We do this by identifying leading and trailing segments that
        have no actual language in them ('like '<b>' or ' ').  Then,
        we *also* identify the prefix and suffix of the remaining text
        that has no actual language in it (the ' hi ' example above).
        Once we've stripped those all off, we call add_underscore() on
        the middle that remains.

        Arguments:
           text_segments: a list of strings, each of which is a single
              entity as returned by the html parser (so a tag, a
              string of text, a jinja2 entity, etc).  We are promised
              that no entity in this list is something that separates
              natural-language runs (such as a '<p>' tag), and none
              is inside <script> or <style> tags.

        Returns:
           A string that is the concatenation of text_segments, with _()
           inserted around it where appropriate.
        """
        # If our string of text consists only of tags,
        # non-natural-language jinja2 functions, or non-text (only
        # whitespace, say), then we can emit it verbatim.
        for segment in text_segments:
            if not self.no_natural_language(segment):
                break
        else:   # if/else
            # If we get here, then all the segments are non-nl segments.
            # But they may be tags with natural-language attributes...
            retval = []
            for segment in text_segments:
                if segment.startswith('<'):
                    retval.append(self.handle_tag(segment))
                else:
                    retval.append(segment)
            return ''.join(retval)

        # Otherwise, we can emit leading tags and non-text
        # (e.g. whitespace) verbatim, and likewise for trailing
        # tags/non-text.  (So for "<b>Foo</b>", we just i18n-ize "Foo".)
        # Everything else -- including jinja2 vars, since we don't know if
        # they're associated with the surrounding text or not -- goes
        # inside the _().  HOWEVER: it looks weird to translators if an
        # end-tag is in translation text, but its corresponding start-tag
        # isn't, so we keep track of how tags match up and make sure they
        # stay together.

        # Assume tags are properly nested.  We *could* check tagnames too...
        tag_map = {}   # segment-holding-start-tag -> segment-holding-end-tag
        tag_stack = []
        for (i, text_segment) in enumerate(text_segments):
            if text_segment.startswith('</'):
                if tag_stack:
                    start_segment = tag_stack.pop()
                    tag_map[start_segment] = i
            elif (text_segment.endswith('/>') or         # open+close together
                  self._segment_is_tag(text_segment, 'br')  or  # has no close
                  self._segment_is_tag(text_segment, 'img')):
                pass                             # a push followed by a pop :)
            elif text_segment.startswith('<'):
                tag_stack.append(i)

        first = 0                       # first 'contentful' segment
        last = len(text_segments) - 1   # last 'contentful' segment
        while (first <= last and
               (text_segments[first].startswith('<') or
                self.NO_NATURAL_LANGUAGE.match(text_segments[first]))):
            first += 1
        while (first <= last and
               (text_segments[last].startswith('<') or
                 self.NO_NATURAL_LANGUAGE.match(text_segments[last]))):
            last -= 1

        # Now modify first and last so that matching tags stay together.
        # At the end of this, all pairs will either be entirely inside
        # [first..last], or entirely outside it.  (This depends on the
        # fact tags nest.)
        for (start_segment, end_segment) in tag_map.iteritems():
            if start_segment >= first and start_segment <= last:
                last = max(last, end_segment)
            if end_segment >= first and end_segment <= last:
                first = min(first, start_segment)

        # The above fails when end-of-sentence punctuation follows a
        # tag: "foo <b>bar</b>?"  In this case, '?' is in a segment of
        # its own, and since it's not natural language, it will be left
        # out of the segments-to-translate, which is bad.  We avoid
        # this by using the looser NO_NATURAL_LANGUAGE_SUFFIX test on
        # the last segment, when the preceding segment is a tag.
        if (last > first and last < len(text_segments) - 1 and
            not self.is_entity(text_segments[last + 1]) and
            self.is_entity(text_segments[last]) and
            not self.NO_NATURAL_LANGUAGE_SUFFIX.match(text_segments[last + 1])
            ):
                last += 1

        pre_segments = text_segments[:first]
        post_segments = text_segments[last + 1:]
        i18n_segments = text_segments[first:(last + 1)]

        # The prefix of the first i18n segment, and the suffix of the last
        # i18n segment, may also be whitespace (or non-text).  e.g.
        # ['  My name!  '].  Move those out of _() as well.
        def move_prefix_out(regexp):
            """If i18n_segments starts with regexp, move it to pre_segments."""
            if i18n_segments and not self.is_entity(i18n_segments[0]):
                m = regexp.match(i18n_segments[0])
                if m:
                    pre_segments.append(i18n_segments[0][:m.end()])
                    i18n_segments[0] = i18n_segments[0][m.end():]

        def move_suffix_out(regexp):
            """If i18n_segments ends with regexp, move it to post_segments."""
            if i18n_segments and not self.is_entity(i18n_segments[-1]):
                m = regexp.search(i18n_segments[-1])
                if m:
                    post_segments.insert(0, i18n_segments[-1][m.start():])
                    i18n_segments[-1] = i18n_segments[-1][:m.start()]

        move_prefix_out(self.NO_NATURAL_LANGUAGE_PREFIX)
        move_suffix_out(self.NO_NATURAL_LANGUAGE_SUFFIX)

        # The *only* difference in our handling of text inside a tag
        # attribute vs outside, is that when we're inside a tag we
        # never i18nize surrounding quotes, since they're probably
        # part of the html markup:
        #    <img alt="hello">   <-- do not include quotes in _(...)
        if self.inside_tag():
            quotes_re_string = '[\'\"]'
            move_prefix_out(re.compile(r'^%s' % quotes_re_string))
            move_suffix_out(re.compile(r'%s$' % quotes_re_string))

        # NO_NATURAL_LANGUAGE_PREFIX and NO_NATURAL_LANGUAGE_SUFFIX
        # are asymmetrical, so it's possible to match ')' on the end
        # of i18n_segments without matching the corresponding '(' at
        # the beginning.  Let's embrace symmetry and correct for that.
        _SYMMETRY_PAIRS = ('""', "''", '()', '[]', '{}', '<>')
        _SYMMETRY_MAP = dict((p[1], p[0]) for p in _SYMMETRY_PAIRS)
        if i18n_segments and not self.is_entity(i18n_segments[-1]):
            prefix = _SYMMETRY_MAP.get(i18n_segments[-1][-1], None)
            if pre_segments and pre_segments[-1][-1] == prefix:
                # Need to move the prefix from pre-segments into i18n_segments
                i18n_segments.insert(0, prefix)
                pre_segments[-1] = pre_segments[-1][:-1]

        converted_text = self.add_underscore(''.join(i18n_segments))

        # We want to i18nize natural-language text inside html tag
        # attributes (e.g. <img alt="hello">.)  Obviously, we don't
        # i18nize tags inside converted_text; it's already i18nized.
        for (i, segment) in enumerate(pre_segments):
            if segment.startswith('<'):
                pre_segments[i] = self.handle_tag(segment)
        for (i, segment) in enumerate(post_segments):
            if segment.startswith('<'):
                post_segments[i] = self.handle_tag(segment)

        return ''.join(pre_segments + [converted_text] + post_segments)

    def handle_segment(self, segment, segment_separates_nltext):
        """The function that HtmlLexer calls back to for each segment.

        When seeing a segment that continues a run of natural-language
        text, we just buffer it.  When we see a segment that separates
        runs of nl-text, we take all the buffered text, add i18n
        markup to it, and emit it.  Then we emit our segment of text --
        which by definition doesn't contain any natural-language
        markup -- unchanged.

        Arguments:
            segment: the text segment.  If it's None, this means that
                the parser has finished with the document (clean-up time).
            segment_separates_nltext: True if HtmlLexer deems this segment
                to separate runs of natural-language text, or False if
                HtmlLexer deems this segment to continue an existing run
                of natural language text (or maybe start a new one).

        Return value:
           '' when buffering, or the buffered-up text at emit-time.
        """
        retval = ''
        if segment is None or segment_separates_nltext:
            # Emit the natural-language text we've been buffering up.
            if self.nltext_segments:
                # This add_i18n() call is the whole point of all this parsing!
                retval += self.add_i18n(self.nltext_segments)
                self.nltext_segments = []

        # Now handle our new segment
        if segment is None:
            pass
        elif segment_separates_nltext:
            # We want to i18nize natural-language text inside html tag.
            if segment.startswith('<'):
                segment = self.handle_tag(segment)
            retval += segment
        else:
            self.nltext_segments.append(segment)

        return retval


class Jinja2TextHandler(NullTextHandler):
    # We want to ignore runs that can't have natural language in them.
    # In addition to punctuation/whitespace/etc, this includes those
    # jinja2 constructs which don't include natural language:
    # {# comments #} and {% blocks %}, and also {{ variables }} and
    # {{ some_function_calls() }} including {{ variables["arg"] }} and
    # {{ fn("")}} and {{ js_css_packages.package("filename") }}.  But
    # not {{ other_function_calls("with quoted text") }}, which may
    # have natural-language function args.
    J2_VAR = re.compile(r'{{((?:}[^}]|[^}])*)}}')
    J2_VAR_NO_NATURAL_LANGUAGE_STRING = (
        r'{{(?:}[^}"]|[^}"]|\["|"\]|""|(?:\b(%s)\([^)]*\)))*}}')
    J2_VAR_NO_NATURAL_LANGUAGE = re.compile(
        J2_VAR_NO_NATURAL_LANGUAGE_STRING % '|'.join(_OK_FUNCTIONS))

    def is_entity(self, segment):
        """Return true if segment is an <html tag>, not a string of text."""
        return (super(Jinja2TextHandler, self).is_entity(segment) or
                segment.startswith('{{'))

    def no_natural_language(self, segment):
        html_handler = super(Jinja2TextHandler, self)
        # We can't just call html_handler.no_natural_language() because
        # it calls is_entity, which for us includes an unwanted test for '{{'.
        return (html_handler.is_entity(segment) or
                html_handler.NO_NATURAL_LANGUAGE.match(segment) or
                self.J2_VAR_NO_NATURAL_LANGUAGE.match(segment))

    def add_underscore(self, s):
        """Add _("...") around s, handling jinja2 variables correctly.

        If s doesn't have any {{jinja2_vars}} in it, this function is
        trivial (though we do need to escape any double-quotes and
        percent-signs inside s).  Otherwise, we need to rewrite from
           Have {{days}} nice days!
        to
           _("Have %(days) nice days!", days=days)

        Arguments:
            s: a text string

        Return value:
            A string that looks like _("<s>") or
            _("<a modification of s>", var=value, var=value)
            In both cases, we backslash-escape double-quotes inside s.

            For constructs we can't handle yet, such as function calls,
            we return _TODO(<s>); we do not escape quotes.
        """
        if not s:
            return ''     # no need to add an underscore to the empty string!

        original_s = s    # only used if we end up using a _TODO

        # Find the jinja2 variables.
        vars = {}   # map from python-name to jinja2 value
        pct_escape_until = len(s)   # we want to escape %'s in the whole string
        # Go backwards so start- and end-pos isn't messed up as we adjust s.
        for m in reversed(list(self.J2_VAR.finditer(s))):
            var = m.group(1).strip()   # include any filters or fn arguments
            # Get the name of the variable or function (sans filter/fn args).
            varname = re.split('[|(]', var, 1)[0]   # |==filter (==fn args
            # Normalize varname so it's a legal python variable name.
            varname = re.sub('\W', '_', varname.strip())

            if (' _("' in var or '(_("' in var or var.startswith('_("') or
                'ngettext(' in var or 'i18n_do_not_translate(' in var):
                # It looks like this var has already been marked for i18n.
                # Leave it alone.
                return s
            elif '"' in var:
                # A function with string args, or a filter with string args.
                # Either way, we'll need a human to i18n-ize the args.
                return '_TODO(%s)' % original_s
            if vars.get(varname, var) != var:
                # Hmm, same varname, but different filters or fn arguments.
                # Someone will have to figure out what's going on manually.
                return '_TODO(%s)' % original_s

            # Replace this var with a python var: {{days}} -> %(days)s
            # We also escape any %'s after this var, since they should be
            # interpreted as literal percent-signs.  We wait until now
            # because we know that nothing after us has jinja2-vars in it,
            # and we don't want to double %'s inside of {{ jinja2_fn_calls }}.
            s = ('%s%%(%s)s%s%s' %
                 (s[:m.start()],
                  varname,
                  s[m.end():pct_escape_until].replace('%', '%%'),
                  s[pct_escape_until:]))
            pct_escape_until = m.start()
            vars[varname] = var

        # Escape any remaining %'s we need to.  We only escape %'s if
        # the string has %(vars)s in it (just like normal python strings).
        if vars:
            s = s[:pct_escape_until].replace('%', '%%') + s[pct_escape_until:]

        # Escape the double-quotes in s.
        escaped_s = s.replace('"', '\\"')

        arglist = [', %s=%s' % var_and_val for var_and_val in vars.iteritems()]
        return '{{ _("%s"%s) }}' % (escaped_s, ''.join(arglist))


class HandlebarsTextHandler(NullTextHandler):
    HANDLEBARS_VAR = re.compile(r'{{((?:}[^}]|[^}])*)}}')
    HANDLEBARS_VAR_NO_NATURAL_LANGUAGE = re.compile(r'{{(?:}[^}"]|[^}"])*}}')

    def is_entity(self, segment):
        """Return true if segment is an <html tag>, not a string of text."""
        return (super(HandlebarsTextHandler, self).is_entity(segment) or
                segment.startswith('{{'))

    def no_natural_language(self, segment):
        html_handler = super(HandlebarsTextHandler, self)
        # We can't just call html_handler.no_natural_language() because
        # it calls is_entity, which for us includes an unwanted test for '{{'.
        return (html_handler.is_entity(segment) or
                html_handler.NO_NATURAL_LANGUAGE.match(segment) or
                self.HANDLEBARS_VAR_NO_NATURAL_LANGUAGE.match(segment))

    def add_underscore(self, s):
        """Add {{#_}}...{{/_}} around s."""
        if not s:
            return ''     # no need to add an underscore to the empty string!
        return '{{#_}}%s{{/_}}' % s


def i18nize(html_file, parser):
    if html_file == '-':
        input = sys.stdin.read()
    else:
        with open(html_file) as f:
            input = f.read()

    parsed_output = parser.parse(input.decode('utf-8')).encode('utf-8')

    if html_file == '-':
        sys.stdout.write(parsed_output)
    else:
        with open(html_file, 'w') as f:  # we modify in-place
            f.write(parsed_output)


def get_parser_for_file(html_file, assume_handlebars=False,
                        debug_parser=False):
    """Return a usable parser for a file named html_file, based on the name."""
    if html_file == '-' and assume_handlebars:
        parser_class = HandlebarsHtmlLexer
        text_handler = HandlebarsTextHandler
    elif html_file.endswith('.handlebars'):
        parser_class = HandlebarsHtmlLexer
        text_handler = HandlebarsTextHandler
    else:
        parser_class = Jinja2HtmlLexer
        text_handler = Jinja2TextHandler
    if debug_parser:
        text_handler = NullTextHandler   # overrides the above

    # We need a second parser to handle nl-text in tag attributes.
    tag_parser = parser_class(text_handler(None).handle_segment)
    parser = parser_class(text_handler(tag_parser).handle_segment)
    return parser


def main(argv=sys.argv):
    arg_parser = argparse.ArgumentParser(
        description=('Auto-add _("...") to jinja2 and handlebars html files'
                     ' (modifying them in place).'))
    arg_parser.add_argument('html_files', nargs='*', default=['-'],
                            metavar='html_file',
                            help=('The HTML files to add _() to.'
                                  ' If none is specified reads from stdin'
                                  ' and writes to stdout.'))
    arg_parser.add_argument('--debug_parser', '-d', action='store_true',
                            help=('Use the null parser, which does not add'
                                  ' any _().  Output should match input.'))
    arg_parser.add_argument('--handlebars', action='store_true',
                            help=('Assume stdin is a handlebars file, not'
                                  ' jinja2 (ignored except for stdin).'))
    args = arg_parser.parse_args(argv[1:])

    num_errors = 0
    for html_file in args.html_files:
        try:
            print >>sys.stderr, 'i18nizing %s' % html_file
            parser = get_parser_for_file(html_file, args.handlebars,
                                         args.debug_parser)
            i18nize(html_file, parser)
        except Exception, why:
            print >>sys.stderr, 'ERROR i18nizing %s: %s' % (html_file, why)
            num_errors += 1
            if len(args.html_files) == 1:
                # For just one file, let it raise the exception if it fails.
                raise

    return num_errors


if __name__ == '__main__':
    sys.exit(min(main(sys.argv), 127))      # 128+ is reserved by the shell
