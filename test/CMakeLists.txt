SET(SOURCE_FILES
	test_main.f90
	../src/operator_type_module.f90
	../src/operator_plus.f90
	../src/operator_devide.f90
	)

ADD_EXECUTABLE(test_main ${SOURCE_FILES})
ADD_TEST(NAME unitTest_main COMMAND test_main)
