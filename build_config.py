"""
This is the place that takes the basic configuration of your LaTeX
build project.
"""

# The name of our main LaTeX source, e. g. 'thesis' or a file 'thesis.tex'.
LATEX_PROJECT = 'thesis'

# Default target.
DEFAULT_TARGET = 'dvi2pdf'

# --- Things below should mostly not need touching. ---
# Some rather fixed configurations.

IMAGES_DIRECTORY = 'figures'
GENERATED_DIRECTORY = 'generated'
CHAPTER_DIRECTORY = 'tex'

FILE_EXTENSIONS = {'eps': '.eps',
                   'pdf': '.pdf',
                   'png': '.png',
                   'jpg': '.jpg',
                   'gnuplot': '.gnuplot'}

MAKEINDEX_EXTENSIONS = ['.glg', '.glo', '.gls']
