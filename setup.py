import os
from setuptools import setup, find_packages

def get_long_description():
    with open(os.path.join(os.path.dirname(__file__), 'README.md')) as f:
        return f.read()

setup(
    name = 'clformat',
    version = __import__('clformat').get_version(),
    url = 'https://github.com/jkfurtney/clformat',
    author = 'Jason Furtney',
    author_email = 'jkfurtney@gmail.com',
    description = "A port of the Common Lisp FORMAT function to Python.",
    long_description = get_long_description(),
    keywords = 'string format lisp',
    tests_require = ['nose'],
    install_requires = [],
    packages = find_packages(),
    include_package_data = True,
    classifiers = [
        'Natural Language :: English',
        'Programming Language :: Lisp',
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 2',
        'License :: OSI Approved :: MIT License',
    ],
    entry_points = {}
)
