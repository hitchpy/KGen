# Makefile for KGEN-generated instrumentation


run: build
	cd /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys; cd /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes; make run

build: replace
	cd /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys; cd /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes; make build
	mv -f /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/kernel.F90.kgen_org /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/kernel.F90
	mv -f /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/types_mod.F90.kgen_org /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/types_mod.F90

replace: save
	cp -f kernel.F90 /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/kernel.F90
	cp -f types_mod.F90 /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/types_mod.F90

recover:
	cp -f /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/kernel.F90.kgen_org /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/kernel.F90
	cp -f /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/types_mod.F90.kgen_org /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/types_mod.F90

recover_from_locals:
	cp -f kernel.F90.kgen_org /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/kernel.F90
	cp -f types_mod.F90.kgen_org /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/types_mod.F90

save:
	if [ ! -f /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/kernel.F90.kgen_org ]; then cp -f /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/kernel.F90 /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/kernel.F90.kgen_org; fi
	if [ ! -f kernel.F90.kgen_org ]; then cp -f /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/kernel.F90 kernel.F90.kgen_org; fi
	if [ ! -f /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/types_mod.F90.kgen_org ]; then cp -f /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/types_mod.F90 /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/types_mod.F90.kgen_org; fi
	if [ ! -f types_mod.F90.kgen_org ]; then cp -f /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes/types_mod.F90 types_mod.F90.kgen_org; fi

clean:
	cd /lustre/pfs1/cades-ccsi/peiy/GlobalVar/KGen/test/gext/func/ys/complex_datatypes; make clean

