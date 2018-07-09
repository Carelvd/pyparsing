#!/usr/bin/env python

"""Setup script for the pyparsing module distribution."""
# from distutils.core import setup
from setuptools import setup, find_packages

import sys
import os

from pyparsing import __version__ as pyparsing_version
    
# modules = ["pyparsing",]

setup(# Distribution meta-data    name = "pyparsing",
    version = pyparsing_version,
    description = "Python parsing module",
    author = "Paul McGuire",
    author_email = "ptmcg@users.sourceforge.net",
    url = "https://github.com/pyparsing/pyparsing/",
    download_url = "https://pypi.org/project/pyparsing/",
    license = "MIT License",
#     py_modules = modules,
    packages = find_packages(exclude=["docs","examples","htmldoc","pyparsing-*"]),
    python_requires='>=2.6, !=3.0.*, !=3.1.*, !=3.2.*',
    command_options      = {'build_sphinx': {
                             'version'    : ('setup.py', "2.0"),
                             'release'    : ('setup.py', "2.0.1"),
                             'source_dir' : ('setup.py','docs'),
                             'build_dir'  : ('setup.py','dist'),
                             'config_dir' : ('setup.py','docs'),}}, 
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Intended Audience :: Developers',
        'Intended Audience :: Information Technology',
        'License :: OSI Approved :: MIT License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.0',
        'Programming Language :: Python :: 3.1',
        'Programming Language :: Python :: 3.2',
        'Programming Language :: Python :: 3.3',
        ]
    )