KGEN_HOME := ../..
KGEN := ${KGEN_HOME}/bin/kgen

SRC_DIR := ${PWD}/src
SRC := ${SRC_DIR}/update_mod.F90
CALLSITE := update_mod:update:calc

test:
	${KGEN} \
		--mpi enable \
		--invocation 0:0:1,0:0:3,1:0:1,1:0:3 \
		--timing repeat=100 \
        --rebuild all \
		--cmd-clean "cd ${SRC_DIR}; make -f Makefile.mpirun clean" \
		--cmd-build "cd ${SRC_DIR}; make -f Makefile.mpirun build" \
        --cmd-run "cd ${SRC_DIR}; make -f Makefile.mpirun run" \
		${SRC}:${CALLSITE}
		#--source alias=/path/a:path/b # add this option if your system requires alias paths

clean:
	${MAKE} -f Makefile.mpirun clean -C src
	rm -rf kernel state kgen.log _kgen_compflag_cmdwrapper.sh strace.log include.ini
