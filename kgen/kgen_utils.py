# kgen_utils.py
# KGEN utillities

import os
import re
import sys
from Fortran2003 import Name, Data_Ref

#############################################################################
## COMMON
#############################################################################

# Put src folder first in path
sys.path = sys.path + [ os.path.dirname(__file__) ]

EXTERNAL_NAMELEVEL_SEPERATOR = ':'
INTERNAL_NAMELEVEL_SEPERATOR = '__kgen__' # lower-case only

def encode_NS(namepath):
    return namepath.replace(EXTERNAL_NAMELEVEL_SEPERATOR, INTERNAL_NAMELEVEL_SEPERATOR)

def decode_NS(namepath):
    return namepath.replace(INTERNAL_NAMELEVEL_SEPERATOR, EXTERNAL_NAMELEVEL_SEPERATOR)

class KGName(object):
    def __init__(self, name, node=None, stmt=None):
        if not name: raise ProgramException('Name can not be none or blank')
        if name[0].isdigit(): raise ProgramException('Name can not have digit as its first character')

        self.namepath = encode_NS(name).strip().lower() # lower case
        self.namelist = self.namepath.split(INTERNAL_NAMELEVEL_SEPERATOR)
        self.dataref = Data_Ref(self.namelist[-1])
        self.node = node
        self.stmt = stmt
        #self.rename = []

    def path(self):
        return decode_NS(self.namepath)

    def list(self):
        return self.namelist

    def dataref(self):
        return self.dataref

    def last(self):
        return self.namelist[-1]

    def first(self):
        return self.namelist[0]

    def firstpartname(self):
        if isinstance(self.dataref, Name):
            return self.dataref.string
        else:
            return self.dataref.items[0].string

    def __eq__(self, other):
        return self.namepath==other.namepath

    def __str__(self):
        raise Exception('KGName')

def get_namepath(stmt):
    return EXTERNAL_NAMELEVEL_SEPERATOR.join([ a.name.lower() for a in stmt.ancestors() ])

def pack_namepath(stmt, lastname):
    return '%s%s%s'%(get_namepath(stmt), EXTERNAL_NAMELEVEL_SEPERATOR, lastname)

def singleton(cls):
    """ singleton generator """

    instances = {}
    def get_instance():
        if cls not in instances:
            instances[cls] = cls()
        return instances[cls]
    return get_instance()

def exec_cmd(cmd, show_error_msg=True):
    import subprocess

    proc = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    out = proc.stdout.read()
    ret_code = proc.wait()
    if ret_code != 0 and show_error_msg:
        cmd_out, cmd_err = proc.communicate()
        print '>> %s' % cmd
        print 'returned non-zero code from shell('+str(ret_code)+')\n OUTPUT: '+str(cmd_out)+'\n ERROR: '+str(cmd_err)+'\n'
    return out

def traverse(node, func, extra, attr='items', prerun=True, depth=0):
    if prerun and func is not None:
        func(node, depth, extra)

    if node and hasattr(node, attr) and getattr(node, attr):
            exec('for child in node.%s: traverse(child, func, extra, attr=attr, prerun=prerun, depth=depth+1)' % attr)

    if not prerun and func is not None:
        func(node, depth, extra)

def get_subtree(obj, tree, prefix='top', depth=0):
    tab = '    '
    postfix = ''
    if isinstance(obj, str): postfix = ' => ' + obj
    elif isinstance(obj, type): postfix = ' => ' + str(obj)
    elif obj.__class__.__name__=='Name': postfix = ' => ' + obj.string

    #tree += [ ( tab*depth + prefix + ': ' + str(obj.__class__) + postfix, depth ) ]
    if hasattr(obj, 'parent'):
        pcls = str(obj.parent.__class__)
    else:
        pcls = 'None'
    tree += [ ( tab*depth + prefix + ': ' + str(obj.__class__) + postfix + ': parent => ' + pcls , depth ) ]
    if hasattr(obj, 'items'):
        for item in obj.items:
            get_subtree(item, tree, prefix='item', depth=depth+1)

    if hasattr(obj, 'content'):
        for elem in obj.content:
            get_subtree(elem, tree, prefix='content', depth=depth+1)

def show_obj(obj):
    print 'CLS: ', obj.__class__
    print 'STR: ', str(obj)
    print 'DIR: ', dir(obj)

def show_tree(node, prevent_print=False):
    tree = []
    get_subtree(node, tree)
    lines = []
    for elem, depth in tree:
        line = '    '*depth + elem
        if not prevent_print:
            print line
        lines.append(line+'\n')
    return lines

#############################################################################
## EXCEPTION
#############################################################################

class KGException(Exception):
    pass

class UserException(KGException):
    pass

class ProgramException(KGException):
    pass

#############################################################################
## CONFIG
#############################################################################

def process_include_option(include_option, incattrs):
    import ConfigParser

    # collect include configuration information
    Inc = ConfigParser.RawConfigParser()
    Inc.optionxform = str
    Inc.read(include_option)
    for section in Inc.sections():
        lsection = section.lower().strip()
        #if lsection in [ 'type', 'rename', 'state', 'extern' ]:
        if lsection in [ 'type', 'macro' ]:
            for option in Inc.options(section):
                incattrs[lsection][option] = Inc.get(section, option).strip()
        elif lsection=='include':
            for option in Inc.options(section):
                incattrs['path'].append(option.strip())
        elif os.path.isfile(section):
            abspath = os.path.abspath(section)
            if not incattrs['file'].has_key(abspath):
                incattrs['file'][abspath] = {}
                incattrs['file'][abspath]['path'] = []
                incattrs['file'][abspath]['macro'] = {}
            for option in Inc.options(section):
                if option=='include':
                    pathlist = Inc.get(section, option).split(':')
                    incattrs['file'][abspath]['path'].extend(pathlist)
                else:
                    incattrs['file'][abspath]['macro'][option] = Inc.get(section, option)
        else:
            print '%s is either not suppored keyword or can not be found. Ignored.' % section

def process_exclude_option(exclude_option, excattrs):
    import ConfigParser

    # collect exclude configuration information
    Exc = ConfigParser.RawConfigParser()
    Exc.optionxform = str
    Exc.read(exclude_option)
    for section in Exc.sections():
        lsection = section.lower().strip()
        excattrs[lsection] = {}
        for option in Exc.options(section):
            loption = option.lower().strip()
            excattrs[lsection][loption] = Exc.get(section, option).strip().split('=')

@singleton
class Config(object):
    """ KGEN configuration parameter holder """

    def __init__(self):
        import optparse

        # setup config parameters
        self._attrs = {}

        # KGEN operation mode
        self._attrs['check_mode'] = False

        # kgen parameters
        self._attrs['kgen'] = {}
        self._attrs['kgen']['version'] = [ 0, 5, '2' ]

        # Fortran parameters
        self._attrs['fort'] = {}
        self._attrs['fort']['maxlinelen'] = 134

        # logging parameters
        self._attrs['logging'] = {}
        self._attrs['logging']['select'] = {}

        # callsite parameters
        self._attrs['callsite'] = {}
        self._attrs['callsite']['filename'] = ''
        self._attrs['callsite']['subpname'] = ''
        self._attrs['callsite']['lineafter'] = -1

        # external tool parameters
        self._attrs['bin'] = {}
        self._attrs['bin']['pp'] = 'fpp'
        self._attrs['bin']['cpp_flags'] = '-w -traditional'
        self._attrs['bin']['fpp_flags'] = '-w'

        # test parameters
        self._attrs['test'] = {}
        self._attrs['test']['suite'] = ''

        # search parameters
        self._attrs['search'] = {}
        self._attrs['search']['skip_intrinsic'] = True
        self._attrs['search']['except'] = []
        self._attrs['search']['promote_exception'] = False

        # path parameters
        self._attrs['path'] = {}
        self._attrs['path']['outdir'] = '.'
        self._attrs['path']['state'] = 'state'
        self._attrs['path']['kernel'] = 'kernel'
        self._attrs['path']['alias'] = {}

        # mpi parameters
        self._attrs['mpi'] = {}
        self._attrs['mpi']['enabled'] = False
        self._attrs['mpi']['ranks'] = [ '0' ]
        self._attrs['mpi']['size'] = len(self._attrs['mpi']['ranks'])
        self._attrs['mpi']['comm'] = 'MPI_COMM_WORLD'
        self._attrs['mpi']['header'] = 'mpif.h'
        self._attrs['mpi']['use_stmts'] = []

        # invocation parameters
        self._attrs['invocation'] = {}
        self._attrs['invocation']['numbers'] = [ '1' ]
        self._attrs['invocation']['size'] = len(self._attrs['invocation']['numbers'])

        # timing parameters
        self._attrs['timing'] = {}
        self._attrs['timing']['repeat'] = 10

        # verification parameters
        self._attrs['verify'] = {}
        self._attrs['verify']['tolerance'] = '1.E-14'
        self._attrs['verify']['verboselevel'] = 1

        # include parameters
        self._attrs['include'] = {}
        self._attrs['include']['macro'] = {}
        self._attrs['include']['path'] = ['.']
        self._attrs['include']['type'] = {}
        self._attrs['include']['file'] = {}

        # exclude parameters
        self._attrs['exclude'] = {}

        # make kernel parameters
        self._attrs['kernel_compile'] = {}
        self._attrs['kernel_compile']['FC'] = 'ifort'
        self._attrs['kernel_compile']['FC_FLAGS'] = ''

        self._attrs['kernel_link'] = {}
        self._attrs['kernel_link']['include'] = []
        self._attrs['kernel_link']['pre_cmds'] = []
        self._attrs['kernel_link']['lib'] = []

        # make state parameters
        self._attrs['state_build'] = {}
        self._attrs['state_build']['cmds'] = ''
        self._attrs['state_run'] = {}
        self._attrs['state_run']['cmds'] = ''
        self._attrs['state_switch'] = {}
        self._attrs['state_switch']['type'] = 'replace'
        self._attrs['state_switch']['cmds'] = ''

        # kernel correctness check parameters
        self._attrs['check'] = {}
        #self._attrs['check']['pert_invar'] = ['*']
        self._attrs['check']['pert_invar'] = []
        self._attrs['check']['pert_lim'] = '1.0E-15'

        # debugging parameters
        self._attrs['debug'] = {}
        self._attrs['debug']['printvar'] = []

        # parsing arguments
        usage = "usage: %prog [options] call-site"
        parser = optparse.OptionParser(usage=usage)
        parser.add_option("-s", "--syntax-check", dest="syntax_check", action='store_true', default=False, help="KGEN Syntax Check Mode")
        parser.add_option("-i", "--include-ini", dest="include_ini", action='store', type='string', default=None, help="information used for analysis")
        parser.add_option("-e", "--exclude-ini", dest="exclude_ini", action='store', type='string', default=None, help="information excluded for analysis")
        parser.add_option("-I", dest="include", action='append', type='string', default=None, help="include path information used for analysis")
        parser.add_option("-D", dest="macro", action='append', type='string', default=None, help="macro information used for analysis")
        parser.add_option("--outdir", dest="outdir", action='store', type='string', default=None, help="path to create outputs")
        parser.add_option("--invocation", dest="invocation", action='store', type='string', default=None, help="Nth invocation of kernel for data collection")
        parser.add_option("--mpi", dest="mpi", action='append', type='string', default=None, help="MPI information for data collection")
        parser.add_option("--timing", dest="timing", action='store', type='string', default=None, help="Timing measurement information")
        parser.add_option("--skip-intrinsic", dest="skip_intrinsic", action='append', type='string', default=None, help="Skip intrinsic procedures during searching")
        parser.add_option("--noskip-intrinsic", dest="noskip_intrinsic", action='append', type='string', default=None, help="Do not skip intrinsic procedures during searching")
        parser.add_option("--kernel-compile", dest="kernel_compile", action='append', type='string', help="Compile information to generate kernel makefile")
        parser.add_option("--kernel-link", dest="kernel_link", action='append', type='string', help="Link information to generate kernel makefile")
        parser.add_option("--state-switch", dest="state_switch", action='append', type='string', help="Specifying how to switch orignal sources with instrumented ones.")
        parser.add_option("--state-build", dest="state_build", action='append', type='string', help="Build information to generate makefile")
        parser.add_option("--state-run", dest="state_run", action='append', type='string', help="Run information to generate makefile")
        #parser.add_option("--alias", dest="alias", action='append', type='string', help="Name aliasing information")
        parser.add_option("--alias", dest="alias", action='append', type='string', help=optparse.SUPPRESS_HELP)
        parser.add_option("--check", dest="check", action='append', type='string', help="Kernel correctness check information")
        parser.add_option("--debug", dest="debug", action='append', type='string', help=optparse.SUPPRESS_HELP)

        opts, args = parser.parse_args()
        if len(args)<1:
            print 'ERROR: No call-site information is provided in command line.'
            sys.exit(-1)

        if opts.syntax_check:
            self._process_analysis_flags(opts)
            self._attrs['check_mode'] = args
            return

        callsite = args[0].split(':', 1)
        if not os.path.isfile(callsite[0]):
            print 'ERROR: %s can not be found.' % callsite[0]
            sys.exit(-1)

        # set callsite filename
        self.callsite['filename'] = callsite[0]

        # read directives from source file
        self.process_directives(callsite[0])

        # set subpname if exists in command line argument
        if len(callsite)==2:
            self.callsite['subpname'] = KGName(callsite[1])
        elif len(callsite)>2:
            print 'ERROR: Unrecognized call-site information(Syntax -> filename[:subprogramname]): %s'%str(callsite)
            sys.exit(-1)

        # read options from command line. overwrite settings from directives
        self.process_commandline(opts)

    def process_directives(self, filename):

        # collect directives
        directs = {}
        with open(filename, 'rb') as f:
            continued = False
            buf = ''
            lineno_start = -1
            for i, line in enumerate(f.readlines()):
                if len(line)==0: continue

                if continued:
                    match_kgenc = re.match(r'^[c!*]\$kgen&\s+(.+)$', line.strip(), re.IGNORECASE)
                    if match_kgenc:
                        line = match_kgenc.group(1).rstrip()
                        if line[-1]=='&':
                            buf += line[:-1]
                        else:
                            buf += line
                            match_direct = re.match(r'^(\w[\w\d]*)\s+(.+)$', buf.strip(), re.IGNORECASE)
                            if match_direct:
                                directs[(lineno_start, i)] = (match_direct.group(1), match_direct.group(2))

                            lineno_start = -1
                            continued = False
                            buf = ''
                    else:
                        print 'ERROR: KGEN directive syntax error: %s'%line
                        sys.exit(-1)
                else:
                    match_kgen = re.match(r'^[c!*]\$kgen\s+(.+)$', line.strip(), re.IGNORECASE)
                    if match_kgen:
                        if match_kgen.group(1)[-1]=='&':
                            buf = match_kgen.group(1)[:-1]         
                            lineno_start = i
                            continued = True
                        else:
                            match_direct = re.match(r'^(\w[\w\d]*)\s+(.+)$', match_kgen.group(1), re.IGNORECASE)
                            if match_direct:
                                directs[(i, i)] = (match_direct.group(1), match_direct.group(2))

        # populate configuration from directives
        for span, (direct, clause) in directs.iteritems():
            if direct.lower()=='callsite':
                self.callsite['subpname'] = KGName(clause)
                self.callsite['lineafter'] = span[1]
            else:
                print 'WARNING: Not supported KGEN directive: %s'%direct

    def _process_analysis_flags(self, opts):

        # check if exists fpp or cpp
        output = ''
        try: output = exec_cmd('which fpp', show_error_msg=False).strip()
        except Exception as e: pass
        if output.endswith('fpp'):
            self.bin['pp'] = output
        else:
            output = ''
            try: output = exec_cmd('which cpp', show_error_msg=False).strip()
            except Exception as e: pass
            if output.endswith('cpp'):
                self.bin['pp'] = output
            else:
                print 'ERROR: neither cpp or fpp is found'
                sys.exit(-1)

        # parsing intrinsic skip option
        if opts.noskip_intrinsic:
            self._attrs['search']['skip_intrinsic'] = False
            for line in opts.noskip_intrinsic:
                for noskip in line.lower().split(','):
                    key, value = noskip.split('=')
                    if key=='except':
                        self._attrs['search']['except'].extend(value.split(':'))
                    else:
                        raise UserException('Unknown noskip_intrinsic option: %s' % comp)

        if opts.skip_intrinsic:
            self._attrs['search']['skip_intrinsic'] = True
            for line in opts.skip_intrinsic:
                for skip in line.lower().split(','):
                    key, value = skip.split('=')
                    if key=='except':
                        self._attrs['search']['except'].extend(value.split(':'))
                    elif key=='add_intrinsic':
                        Intrinsic_Procedures.extend([name.lower() for name in value.split(':')])
                    else:
                        raise UserException('Unknown skip_intrinsic option: %s' % comp)

        # parsing include parameters
        if opts.include:
            for inc in opts.include:
                inc_eq = inc.split('=')
                if len(inc_eq)==1:
                    for inc_colon in inc_eq[0].split(':'): 
                        self._attrs['include']['path'].append(inc_colon)
                elif len(inc_eq)==2:
                    # TODO: support path for each file
                    pass
                else: raise UserException('Wrong format include: %s'%inc)
 
        if opts.include_ini:
            process_include_option(opts.include_ini, self._attrs['include'])

        if opts.exclude_ini:
            process_exclude_option(opts.exclude_ini, self._attrs['exclude'])

        # parsing macro parameters
        if opts.macro:
            for line in opts.macro:
                for macro in line.split(','): 
                    macro_eq = macro.split('=')
                    if len(macro_eq)==1:
                        self._attrs['include']['macro'][macro_eq[0]] = '1'
                    elif len(macro_eq)==2:
                        self._attrs['include']['macro'][macro_eq[0]] = macro_eq[1]
                    else: raise UserException('Wrong format include: %s'%inc)

    def process_commandline(self, opts):

        self._process_analysis_flags(opts)

        # parsing invocation parameters
        if opts.invocation:
            self._attrs['invocation']['numbers'] = []
            for ord in opts.invocation.split(','):
                if ord.isdigit():
                    self._attrs['invocation']['numbers'].append(ord)
            self._attrs['invocation']['numbers'].sort()
            self._attrs['invocation']['size'] = len(self._attrs['invocation']['numbers'])

        # parsing MPI parameters
        if opts.mpi:
            self._attrs['mpi']['enabled'] = True
            for line in opts.mpi:
                for mpi in line.split(','):
                    key, value = mpi.split('=')
                    if key=='comm':
                        self._attrs['mpi'][key] = value
                    elif key=='use':
                        mod_name, identifier = value.split(':')
                        self._attrs['mpi']['use_stmts'].append('USE %s, only : %s'% \
                            (mod_name, identifier))
                    elif key=='ranks':
                        self._attrs['mpi'][key] = value.split(':')
                        self._attrs['mpi']['size'] = len(self._attrs['mpi'][key])
                    elif key=='header':
                        self._attrs['mpi'][key] = value
                    else:
                        raise UserException('Unknown MPI option: %s' % comp)

        # parsing kernel makefile parameters
        if opts.kernel_compile:
            for line in opts.kernel_compile:
                for comp in line.split(','):
                    key, value = comp.split('=')
                    if key in [ 'FC', 'FC_FLAGS' ] :
                        self._attrs['kernel_compile'][key] = value
                    else:
                        raise UserException('Unknown kernel compile option: %s' % comp)

        if opts.kernel_link:
            for line in opts.kernel_link:
                for link in line.split(','):
                    key, value = link.split('=')
                    if key in [ 'include', 'lib', 'pre_cmds' ] :
                        self._attrs['kernel_link'][key].append(value)
                    else:
                        raise UserException('Unknown kernel link option: %s' % comp)

        if opts.state_build:
            for line in opts.state_build:
                for build in line.split(','):
                    key, value = build.split('=')
                    if key in [ 'cmds' ] :
                        self._attrs['state_build'][key] = value
                    else:
                        raise UserException('Unknown state-build option: %s' % build)

        if opts.state_run:
            for line in opts.state_run:
                for run in line.split(','):
                    key, value = run.split('=')
                    if key in [ 'cmds' ] :
                        self._attrs['state_run'][key] = value
                    else:
                        raise UserException('Unknown state-run option: %s' % run)

        if opts.state_switch:
            for line in opts.state_switch:
                for run in line.split(','):
                    key, value = run.split('=')
                    if key in [ 'cmds', 'type' ] :
                        self._attrs['state_switch'][key] = value
                    else:
                        raise UserException('Unknown state-switch option: %s' % run)

        if opts.timing:
            for time in opts.timing.split(','):
                key, value = time.split('=')
                if key in [ 'repeat' ] :
                    try:
                        self._attrs['timing'][key] = int(value)
                    except:
                        raise UserException('repeat sub-flag should be integer value: %s'%value)
                else:
                    raise UserException('Unknown timing option: %s' % time)

        if opts.outdir:
            self._attrs['path']['outdir'] = opts.outdir

        # create state directories and change working directory
        if not os.path.exists(self._attrs['path']['outdir']):
            os.makedirs(self._attrs['path']['outdir'])
        os.chdir(self._attrs['path']['outdir'])

        # path nicknames
        if opts.alias:
            for line in opts.alias:
                for pathalias in line.split(','):
                    key, value = pathalias.split('=')
                    self._attrs['path']['alias'][key] = value

        # kernel correctness checks 
        if opts.check:
            for line in opts.check:
                for checkparams in line.split(','):
                    key, value = checkparams.split('=')
                    key = key.lower()
                    value = value.lower()
                    if key=='pert_invar':
                        self._attrs['check'][key] = value.split(':')
                    elif key=='pert_lim':
                        self._attrs['check'][key] = value
                    else:
                        print 'WARNING: %s is not supported check parameter'%key

        # parsing debugging options
        if opts.debug:
            for dbg in opts.debug:
                param_path, value = dbg.split('=')
                param_split = param_path.lower().split('.')
                value_split = value.lower().split(',')
                curdict = self._attrs['debug']
                for param in param_split[:-1]:
                    curdict = curdict[param] 
                exec('curdict[param_split[-1]] = value_split')

    def __getattr__(self, name):
        return self._attrs[name]

#############################################################################
## LOGGING
#############################################################################

def check_logging(func):
    """ logging decorator to check if to continue to log """

    def func_wrapper(obj, msg, **kwargs):
        exe_func = True
        if Config.logging['select'].has_key('name'):
            exe_func = False
            if kwargs.has_key('name'):
                np1 = kwargs['name'].namepath.lower()
                for np2 in Config.logging['select']['name']:
                    np1_split = decode_NS(np1).split('.')
                    np2_split = np2.split('.')
                    minlen = min(len(np1_split), len(np2_split))
                    if np1_split[-1*minlen:]==np2_split[-1*minlen:]:
                        exe_func = True
                        break

        if kwargs.has_key('stmt'):
            stmt = kwargs['stmt']
            msg += ' at %s in %s' % ( str(stmt.item.span), stmt.item.reader.id )

        # prerun
        if kwargs.has_key('stdout') and kwargs['stdout']:
            print msg

        # execute func
        if exe_func or func.__name__ in [ 'error', 'critical']:
            func(obj, msg)

        # postrun

    return func_wrapper

@singleton
class Logger(object):
    """ KGEN logger """

    def __init__(self):
        import logging.config
        logconfig_path = os.path.join(os.path.dirname(__file__),'log.config')
        logging.config.fileConfig(logconfig_path)
        self.logger = logging.getLogger('kgen')

    def _pack_msg(self, msg):
        import traceback
        import inspect

        exc_type, exc_value, exc_traceback = sys.exc_info()
        tb = traceback.format_tb(exc_traceback)
        if len(tb)>0:
            return str(msg) + '\n' + '\n'.join(tb)
        else:
            frame=inspect.currentframe()
            frame=frame.f_back.f_back.f_back
            code=frame.f_code
            return '%s:%d - %s'%(os.path.basename(code.co_filename), frame.f_lineno, str(msg))

    @check_logging
    def debug(self, msg, **kwargs):
        self.logger.debug(self._pack_msg(msg))

    @check_logging
    def info(self, msg, **kwargs):
        self.logger.info(self._pack_msg(msg))

    @check_logging
    def warn(self, msg, **kwargs):
        self.logger.warn(self._pack_msg(msg))

    @check_logging
    def error(self, msg, **kwargs):
        self.logger.error(self._pack_msg(msg))

    @check_logging
    def critical(self, msg, **kwargs):
        self.logger.critical(self._pack_msg(msg))

    def exception(self, msg, **kwargs):
        import traceback
        exc_type, exc_value, exc_traceback = sys.exc_info()
        output = [ msg+'\n', '\n' ]
        output += traceback.format_tb(exc_traceback)
        if kwargs.has_key('node') and kwargs['node']:
            output += [ '\n' ] + show_tree(kwargs['node'], prevent_print=True)
        self.logger.info(''.join(output))

import unittest
class Test_kgen_utils(unittest.TestCase):
 
    def setUp(self):
        pass
 
    def test_exec_cmd(self):
        output = exec_cmd('echo "TestOK"')
        self.assertEqual( output, "TestOK\n")
 

if __name__ == "__main__":
    #unittest.main(argv=[sys.argv[0]], verbosity=2) # verbosity is available from Python 2.7
    unittest.main(argv=[sys.argv[0]])
