# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.16

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = "C:/Program Files/CMake/bin/cmake.exe"

# The command to remove a file.
RM = "C:/Program Files/CMake/bin/cmake.exe" -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = C:/Users/mr.xul/Desktop/project

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = C:/Users/mr.xul/Desktop/project/build

# Include any dependencies generated for this target.
include test/CMakeFiles/test_main.dir/depend.make

# Include the progress variables for this target.
include test/CMakeFiles/test_main.dir/progress.make

# Include the compile flags for this target's objects.
include test/CMakeFiles/test_main.dir/flags.make

test/CMakeFiles/test_main.dir/test_main.f90.obj: test/CMakeFiles/test_main.dir/flags.make
test/CMakeFiles/test_main.dir/test_main.f90.obj: ../test/test_main.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=C:/Users/mr.xul/Desktop/project/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object test/CMakeFiles/test_main.dir/test_main.f90.obj"
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c C:/Users/mr.xul/Desktop/project/test/test_main.f90 -o CMakeFiles/test_main.dir/test_main.f90.obj

test/CMakeFiles/test_main.dir/test_main.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/test_main.dir/test_main.f90.i"
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E C:/Users/mr.xul/Desktop/project/test/test_main.f90 > CMakeFiles/test_main.dir/test_main.f90.i

test/CMakeFiles/test_main.dir/test_main.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/test_main.dir/test_main.f90.s"
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S C:/Users/mr.xul/Desktop/project/test/test_main.f90 -o CMakeFiles/test_main.dir/test_main.f90.s

test/CMakeFiles/test_main.dir/__/src/operator_type_module.f90.obj: test/CMakeFiles/test_main.dir/flags.make
test/CMakeFiles/test_main.dir/__/src/operator_type_module.f90.obj: ../src/operator_type_module.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=C:/Users/mr.xul/Desktop/project/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object test/CMakeFiles/test_main.dir/__/src/operator_type_module.f90.obj"
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c C:/Users/mr.xul/Desktop/project/src/operator_type_module.f90 -o CMakeFiles/test_main.dir/__/src/operator_type_module.f90.obj

test/CMakeFiles/test_main.dir/__/src/operator_type_module.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/test_main.dir/__/src/operator_type_module.f90.i"
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E C:/Users/mr.xul/Desktop/project/src/operator_type_module.f90 > CMakeFiles/test_main.dir/__/src/operator_type_module.f90.i

test/CMakeFiles/test_main.dir/__/src/operator_type_module.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/test_main.dir/__/src/operator_type_module.f90.s"
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S C:/Users/mr.xul/Desktop/project/src/operator_type_module.f90 -o CMakeFiles/test_main.dir/__/src/operator_type_module.f90.s

test/CMakeFiles/test_main.dir/__/src/operator_plus.f90.obj: test/CMakeFiles/test_main.dir/flags.make
test/CMakeFiles/test_main.dir/__/src/operator_plus.f90.obj: ../src/operator_plus.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=C:/Users/mr.xul/Desktop/project/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building Fortran object test/CMakeFiles/test_main.dir/__/src/operator_plus.f90.obj"
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c C:/Users/mr.xul/Desktop/project/src/operator_plus.f90 -o CMakeFiles/test_main.dir/__/src/operator_plus.f90.obj

test/CMakeFiles/test_main.dir/__/src/operator_plus.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/test_main.dir/__/src/operator_plus.f90.i"
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E C:/Users/mr.xul/Desktop/project/src/operator_plus.f90 > CMakeFiles/test_main.dir/__/src/operator_plus.f90.i

test/CMakeFiles/test_main.dir/__/src/operator_plus.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/test_main.dir/__/src/operator_plus.f90.s"
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S C:/Users/mr.xul/Desktop/project/src/operator_plus.f90 -o CMakeFiles/test_main.dir/__/src/operator_plus.f90.s

test/CMakeFiles/test_main.dir/__/src/operator_devide.f90.obj: test/CMakeFiles/test_main.dir/flags.make
test/CMakeFiles/test_main.dir/__/src/operator_devide.f90.obj: ../src/operator_devide.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=C:/Users/mr.xul/Desktop/project/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building Fortran object test/CMakeFiles/test_main.dir/__/src/operator_devide.f90.obj"
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c C:/Users/mr.xul/Desktop/project/src/operator_devide.f90 -o CMakeFiles/test_main.dir/__/src/operator_devide.f90.obj

test/CMakeFiles/test_main.dir/__/src/operator_devide.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/test_main.dir/__/src/operator_devide.f90.i"
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E C:/Users/mr.xul/Desktop/project/src/operator_devide.f90 > CMakeFiles/test_main.dir/__/src/operator_devide.f90.i

test/CMakeFiles/test_main.dir/__/src/operator_devide.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/test_main.dir/__/src/operator_devide.f90.s"
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S C:/Users/mr.xul/Desktop/project/src/operator_devide.f90 -o CMakeFiles/test_main.dir/__/src/operator_devide.f90.s

# Object files for target test_main
test_main_OBJECTS = \
"CMakeFiles/test_main.dir/test_main.f90.obj" \
"CMakeFiles/test_main.dir/__/src/operator_type_module.f90.obj" \
"CMakeFiles/test_main.dir/__/src/operator_plus.f90.obj" \
"CMakeFiles/test_main.dir/__/src/operator_devide.f90.obj"

# External object files for target test_main
test_main_EXTERNAL_OBJECTS =

test/test_main.exe: test/CMakeFiles/test_main.dir/test_main.f90.obj
test/test_main.exe: test/CMakeFiles/test_main.dir/__/src/operator_type_module.f90.obj
test/test_main.exe: test/CMakeFiles/test_main.dir/__/src/operator_plus.f90.obj
test/test_main.exe: test/CMakeFiles/test_main.dir/__/src/operator_devide.f90.obj
test/test_main.exe: test/CMakeFiles/test_main.dir/build.make
test/test_main.exe: test/CMakeFiles/test_main.dir/objects1.rsp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=C:/Users/mr.xul/Desktop/project/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Linking Fortran executable test_main.exe"
	cd C:/Users/mr.xul/Desktop/project/build/test && "C:/Program Files/CMake/bin/cmake.exe" -E remove -f CMakeFiles/test_main.dir/objects.a
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/ar.exe cr CMakeFiles/test_main.dir/objects.a @CMakeFiles/test_main.dir/objects1.rsp
	cd C:/Users/mr.xul/Desktop/project/build/test && C:/MinGW/bin/gfortran.exe    -Wl,--whole-archive CMakeFiles/test_main.dir/objects.a -Wl,--no-whole-archive  -o test_main.exe -Wl,--out-implib,libtest_main.dll.a -Wl,--major-image-version,0,--minor-image-version,0 

# Rule to build all files generated by this target.
test/CMakeFiles/test_main.dir/build: test/test_main.exe

.PHONY : test/CMakeFiles/test_main.dir/build

test/CMakeFiles/test_main.dir/clean:
	cd C:/Users/mr.xul/Desktop/project/build/test && $(CMAKE_COMMAND) -P CMakeFiles/test_main.dir/cmake_clean.cmake
.PHONY : test/CMakeFiles/test_main.dir/clean

test/CMakeFiles/test_main.dir/depend:
	$(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" C:/Users/mr.xul/Desktop/project C:/Users/mr.xul/Desktop/project/test C:/Users/mr.xul/Desktop/project/build C:/Users/mr.xul/Desktop/project/build/test C:/Users/mr.xul/Desktop/project/build/test/CMakeFiles/test_main.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : test/CMakeFiles/test_main.dir/depend

