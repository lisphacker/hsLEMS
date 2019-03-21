LEMS_EXAMPLE_DIR = ../LEMS/examples
build:
	stack build

ex4:	build
	stack exec lemsc -- -I ${LEMS_EXAMPLE_DIR} ${LEMS_EXAMPLE_DIR}/example4.xml
