#lang scribble/manual
@require[@for-label[uri-template
                    racket/base]]

@title[#:style "toc"]{uri-template: A template language for URIs (IETF RFC 6570)}
@author[(author+email "Jesse Alama" "jesse@lisp.sh")]

@defmodule[uri-template]

This package aims to implement RFC 6570, URI Template. That RFC specifies a little pattern langauge for specifying classes of URIs. URIs may @emph{match} a URI Template, and given an assignment of values to variables, one may @emph{fill in} a URI Template, which yields a URI.
