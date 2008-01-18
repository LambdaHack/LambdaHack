
AC_INIT(Curses.hsc)
AC_CONFIG_HEADERS([cbits/config.h])
AC_PREREQ([2.52])

# Some libs we need
AC_CHECK_LIB(curses,  addnstr)
AC_CHECK_FUNCS(use_default_colors)
AC_CHECK_HEADERS([regex.h sys/types.h])

# And now a trick to get the current patch count:
if test -d "_darcs" ; then
    PATCH_COUNT_=`darcs changes --xml-output | sed -n '/^<patch/p;/TAG/q' | wc -l | sed 's/ *//g'`
else
    PATCH_COUNT_=""
fi
AC_DEFINE_UNQUOTED(PATCH_COUNT, "$PATCH_COUNT_", [Current patch count])

AC_OUTPUT
