# FIXME: this approach has some issues so it's put on standby for now

# Tales needs a patched Bisonc++ for its "type-safe union" approach which avoids heap allocations
# It merely adds new $insert directives for skeleton and a %polymorphic-additional directive for grammar
# files that is used to specify which types the

# The type-safe union uses C++11's constexpr to determine at compile time the maximum size,
# which is why it cannot be done within the previous iterations of C++.

# include(ExternalProject)
#
# set(BISONCPP_EXECUTABLE "${CMAKE_CURRENT_BINARY_DIR}/bisonc++")
# set(BISONCPP_VERSION "4.03.00")
#
# include(BISONCPP)
#
# ExternalProject_Add(bisonc++
# 	URL "http://sourceforge.net/projects/bisoncpp/files/bisonc%2B%2B/${BISONCPP_VERSION}/bisonc%2B%2B_${BISONCPP_VERSION}.orig.tar.gz"
# 	URL_HASH SHA1=079c9c54b1b16d651be8753b02dafc8a4f16b02e
#	PATCH_COMMAND patch -p1 < "${CMAKE_CURRENT_SOURCE_DIR}/bisonc++-${BISONCPP_VERSION}_Tales.patch"
# 	BUILD_IN_SOURCE 1
# 	CONFIGURE_COMMAND ""
# 	BUILD_COMMAND <SOURCE_DIR>/build program strip
# 	INSTALL_COMMAND <SOURCE_DIR>/build install program ${BISONCPP_EXECUTABLE}
# )
#
# file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/skeletons")
#
# ExternalProject_Add_Step(bisonc++ installskel
# 	COMMAND <SOURCE_DIR>/build install skel ${CMAKE_CURRENT_BINARY_DIR}/skeletons
# 	DEPENDEES install
# 	WORKING_DIRECTORY <SOURCE_DIR>
# )
