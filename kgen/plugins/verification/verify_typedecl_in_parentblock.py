# gen_write_typedecl_in_parentblock.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin
from verify_utils import VERIFY_PBLOCK_LOCALS, get_typedecl_verifyname, get_dtype_verifyname
from verify_subr import create_verify_subr

class Verify_Typedecl_In_Parentblock(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

        self.verify_locals = []
        self.verify_parentblock_subrnames = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register event per typedecl 
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            typedecl_statements.TypeDeclarationStatement, self.is_out_locals_in_parentblock, self.create_subr_verify_typedecl_in_parentblock) 

    def is_out_locals_in_parentblock(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo') and len(node.kgen_stmt.geninfo)>0 and \
            KGGenType.has_state_out(node.kgen_stmt.geninfo) and node.kgen_parent.kgen_stmt==getinfo('parentblock_stmt'):
            return True
        return False

    def create_subr_verify_typedecl_in_parentblock(self, node):
        stmt = node.kgen_stmt
        entity_names = set([ uname.firstpartname() for uname, req in KGGenType.get_state_out(stmt.geninfo)])
        for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
            if entity_name in self.verify_locals: continue

            if hasattr(stmt, 'exclude_names'):
                skip_verify = False
                for exclude_name, actions in stmt.exclude_names.iteritems():
                    if exclude_name==entity_name and 'remove_state' in actions:
                        skip_verify = True
                        break
                if skip_verify: continue

            self.verify_locals.append(entity_name)

            var = stmt.get_variable(entity_name)
            subrname = get_typedecl_verifyname(stmt, entity_name)

            if subrname not in self.verify_parentblock_subrnames:
                self.verify_parentblock_subrnames.append(subrname)

                if stmt.is_derived():
                    if var.is_pointer() or var.is_array():
                        create_verify_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                    else:
                        for uname, req in stmt.unknowns.iteritems():
                            if uname.firstpartname()==stmt.name:
                                subrname = get_dtype_verifyname(req.res_stmts[0])
                                break
                else:
                    create_verify_subr(subrname, entity_name, node.kgen_parent, var, stmt)

            attrs = {'designator': subrname, 'items': ['"%s"'%entity_name, 'check_status', entity_name, 'kgenref_%s'%entity_name]}
            namedpart_append_genknode(node.kgen_kernel_id, VERIFY_PBLOCK_LOCALS, statements.Call, attrs=attrs)

