#lang scribble/manual
@require[@for-label[uri-template
                    racket/base
		    racket/contract]]

@title{Library interface}

@defproc[
(uri-template?
[thing any/c])
boolean?]

Checks whether a value is (a) a string that (b) adheres to the official syntax of URI Template.

@defproc[
(value?
[thing any/c])
boolean?]

Checks whether a (Racket) value is an acceptable value for a (URI Template) variable. There are three kinds of acceptable values: strings, lists, and assocative arrays. These work out, in Racket, to:

@itemlist[
  @item{@emph{string} A (Racket) string (including the empty string)}
  @item{@emph{list} A (Racket) list of (Racket) strings}
  @item{@emph{associative array} A (Racket) list of (Racket) lists of (Racket) strings, each element of which has exactly two members. (Think @racket[assoc].)}
]

@defproc[
(assignment?
[assn any/c])
boolean?]

Checks whether a value is an assignment (of values to variables). Assignments are supposed to be hashes whose keys are strings and whose values are acceptable values (in the sense of @racket[value?]).

@defproc[
(expand-template
[template string?]
[assignment assignment?]
)
string?]

Given a string and an assignment, apply the template. The result is a string.

Naturally, the template should adhere to the syntax of URI Template; otherwise, an error (of type @racket[exn:fail?]) will be thrown.

References to undefined variables do not lead to an error. Such variables will be silently ignored, according to RFC 6570.

Attempts to do syntactically unobjectionable but semantically bad things, such as applying the max-length modifier—something that makes sense only for strings—to a list or associative array, will lead to error (of type @racket[exn:fail:contract?]).

@defproc[
(variables-of
[template string?]
)
(listof string?)]

Find all variables occuring in a template. The result is a list of unique strings.

No promises is made about the order of the variables appearing in the result. The first variable appearing in the template might occur at the beginning, middle, or end of the list.

As with @tt{expand-template}, the given string should adhere to the syntax of URI Template; otherwise, an error (of type @racket[exn:fail?]) will be thrown.
