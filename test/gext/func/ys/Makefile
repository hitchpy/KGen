KGEN_HOME := ${CCSI_DIR}/peiy/GlobalVar/KGen
KGEN := ${KGEN_HOME}/bin/gext

SRC_DIR := ${PWD}/complex_datatypes
SRC := ${SRC_DIR}/calling_module.F90
CALLSITE := calling_module:calling_subroutine:add

test:
	${KGEN} \
		--invocation 0:0:0 \
		--cmd-clean "cd ${SRC_DIR}; make clean" \
		--cmd-build "cd ${SRC_DIR}; make build" \
		--cmd-run "cd ${SRC_DIR}; make run" \
		${SRC}:${CALLSITE} 
#--source alias=/path/a:path/b # add this option if your system requires alias paths

clean:
	${MAKE} clean -C complex_datatypes
	rm -rf kernel state kgen.log strace.log include.ini _kgen_compflag_cmdwrapper.sh
