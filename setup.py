#!/usr/bin/env/python

"""
i18nize-templates
=================

A tool to automatically add i18n markup to jinja2 and handlebars
templates.  It may also work for django, though this is not tested.

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
    i18nize_templates <file> ...
OR
    i18nize_templates [--handlebars] < <infile> > <outfile>
"""

try:
    from setuptools import setup
    extra_arguments = {
        'entry_points': {
            'console_scripts': (
                ['i18nize-templates = i18nize_templates.__init__:main']),
            },
        'test_suite': 'tests',
        }
except ImportError:
    from distutils.core import setup
    extra_arguments = {
        'scripts': ['i18nize-templates']
        }

setup(
    name='i18nize_templates',
    version='0.1',
    url='http://github.com/csilvers/i18nize_templates',
    license='MIT',
    author='Craig Silverstein',
    author_email='csilvers+i18nize_templates@gmail.com',
    description='Adds i18n markup to jinja2 and handlebars templates.',
    long_description=__doc__,
    keywords='i18n jinja jinja2 handlebars translation',
    packages=['i18nize_templates'],
    platforms='any',
    classifiers=[
        'Development Status :: 4 - Beta',
        'Environment :: Web Environment',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Topic :: Software Development :: Internationalization',
        'Topic :: Software Development :: Libraries :: Python Modules'
    ],
    **extra_arguments
)
