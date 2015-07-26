AUTOMAKE_OPTIONS = subdir-objects
AM_CPPFLAGS = -I./pb -I@LIBTRACE_DIR@/src/common -I@LIBTRACE_DIR@/protobuf/src
# enable PIC for x64 support
AM_CXXFLAGS = -fPIC -DPIC

lib_LIBRARIES = libtrace.a
libtrace_a_SOURCES = pb/frame.piqi.pb.h pb/frame.piqi.pb.cc trace.container.cpp

# Don't auto-link LIBS coming from autoconf, because we might not have
# 32-bit equivalents!
LIBS =

SUBDIRS = . utils
