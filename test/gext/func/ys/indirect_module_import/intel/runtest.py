from gext_func_ys_indirect_module_import_test import GExtFuncYSIMITest

class CustomTest(GExtFuncYSIMITest):
    def config(self, myname, result):

        result[myname]['prerun_build'] = 'module swap intel intel/16.0.1'
        result[myname]['prerun_run'] = 'module swap intel intel/16.0.1'

        self.set_status(result, myname, self.PASSED)

        return result

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
