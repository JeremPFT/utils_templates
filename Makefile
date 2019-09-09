PRJ=templates.gpr

all: test

compil::
	gprbuild.exe -j4 -g -gnatef $(PRJ)

clean::
	gprclean.exe $(PRJ)

test: compil
	cd templates_test && make run
