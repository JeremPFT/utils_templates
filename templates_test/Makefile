PRJ=templates_test.gpr
EXE=templates-test-run.exe

all: run

compil::
	gprbuild.exe -j4 -g -gnatef $(PRJ)

clean::
	gprclean.exe $(PRJ)

run: compil
	bin/$(EXE)
