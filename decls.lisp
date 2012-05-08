(cl:in-package :cl-user)

(declaim
 (ftype (function (t &rest t) (values function &optional)) fmt:with-width)
 (ftype (function (t &rest t) (values function &optional)) fmt:trim)
 (ftype (function (t &rest t) (values function &optional)) fmt:comma-char)
 (ftype (function (t &rest t) *) fmt:decimal-align)
 (ftype (function (t) (values function &optional)) fmt:space-to)
 (ftype (function (t) *) fmt:wrt) (ftype (function (t) *) fmt:dsp)
 (ftype (function (function list &rest t) (values function &optional)) fmt:fmt-join)
 (ftype (function (t &rest t) (values function &optional)) fmt:slashified)
 (ftype (function (t &rest t) (values function &optional)) fmt:num)
 (ftype (function (t) (values function &optional)) fmt:num/roman)
 (ftype (function (t &rest t) *) fmt:trim/left)
 (ftype (function (t t &rest t) (values function &optional)) fmt:fmt-bind)
 (ftype (function (t &rest t) *) fmt:decimal-char)
 (ftype (function (t) (values function &optional)) fmt:num/old-roman)
 (ftype (function (t &rest t) *) fmt:radix) (ftype (function (t) *) fmt:nl)
 (ftype (function (t t &rest t) *) fmt:fmt-join/prefix)
 (ftype (function (&rest t) *) fmt:cat)
 (ftype (function (t) *) fmt:fl)
 (ftype (function (t) (values function &optional)) fmt:apply-cat)
 (ftype (function (t t t &rest t) (values function &optional)) fmt:fmt-join/last)
 (ftype (function (t &rest t) *) fmt:fit)
 (ftype (function (t &rest t) (values (or string (member :undef)) &optional)) fmt:fmt)
 (ftype (function (t t &rest t) *) fmt:num/fit)
 (ftype (function (t t &rest t) *) fmt:fmt-join/suffix)
 (ftype (function * (values function &optional)) fmt:tab-to)
 (ftype (function (t &rest t) *) fmt:num/si)
 (ftype (function (t t t &rest t) (values function &optional)) fmt:fmt-join/dot)
 (ftype (function (t &rest t) *) fmt:pad/left)
 (ftype (function (t) (values t &optional)) fmt:fmt-null)
 (ftype (function (t &rest t) *) fmt:pad/both)
 (ftype (function (t &rest t) *) fmt:fit/both)
 (ftype (function (t) *) fmt:wrt/unshared)
 (ftype (function (t &rest t) (values function &optional)) fmt:pad)
 (ftype (function (t &rest t) (values function &optional)) fmt:num/comma)
 (ftype (function (t t &rest t) (values function &optional)) fmt:fmt-let)
 (ftype (function (t &rest t) (values function &optional)) fmt:trim/length)
 (ftype (function (t &rest t) *) fmt:trim/both)
 (ftype (function (t t &rest t) (values function &optional)) fmt:fmt-if)
 (ftype (function (t &rest t) *) fmt:pad-char)
 (ftype (function (t t &rest t) *) fmt:maybe-slashified)
 (ftype (function (t &rest t) *) fmt:fix)
 (ftype (function (t &rest t) *) fmt:fit/left))