AC_INIT([statvfs], [0.1], [gergely@risko.hu], [statvfs])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([statvfs.cabal])

AC_CONFIG_HEADERS([include/StatVFSConfig.h])

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

dnl ** check for full ANSI header (.h) files
AC_HEADER_STDC

# check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([sys/statvfs.h])

# Enable large file support. Do this before testing the types ino_t, off_t, and
# rlim_t, because it will affect the result of that test.
AC_SYS_LARGEFILE

AC_CHECK_FUNCS([statvfs])

# map standard C types and ISO types to Haskell types
FPTOOLS_CHECK_HTYPE(fsblkcnt_t)
FPTOOLS_CHECK_HTYPE(fsfilcnt_t)

AC_OUTPUT
