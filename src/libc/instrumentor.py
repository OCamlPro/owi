import sys
import tempfile
import logging

from pycparser import c_ast, parse_file
from pycparser.c_generator import CGenerator
from pycparser.plyparser import ParseError

sys.setrecursionlimit(10000)

log = logging.getLogger(__name__)

class MethodNotImplemented(Exception):
    """Exception raised when visit method not available"""

    def __init__(self, name):
        self.name = name
        self.message = f"visit_{name}: method not implemented"
        super().__init__(self.message)

class ParsingError(Exception):

    def __init__(self, message):
        self.message = message
        super().__init__(self.message)

def instrument(file : str, includes : list[str]) -> None:
    visitor = BinopVisitor()

    includes = list(map(lambda inc: f"-I{inc}", includes))
    try:
        ast = parse_file(
            file,
            use_cpp=True,
            cpp_path="clang",
            cpp_args=[r"-E"] + includes
        )
    except ParseError as e:
        raise ParsingError(str(e))

    n_ast = visitor.to_string(visitor.visit(ast))
    with open(file, 'w') as f:
        f.write(n_ast)

class BinopVisitor(c_ast.NodeVisitor):

    def __init__(self, rm_boolops=True):
        self.counter = 0
        self.rm_boolops = rm_boolops
        self.ctx = [ "" ]

    def _safe_visit(self, node):
        return self.visit(node) if node is not None else node

    def _safe_map(self, f, lst):
        return map(f, lst) if lst is not None else lst

    def _fresh_int(self):
        fresh = self.counter
        self.counter += 1
        return fresh

    def _get_binop_func(self, op):
        if op == "&&":
            return "and_"
        elif op == "||":
            return "or_"
        else:
            raise RuntimeError

    def to_string(self, node):
        return CGenerator().visit(node)

    # def visit_ArrayDecl(self, node):
    #     return node

    # def visit_ArrayRef(self, node):
    #     return node

    def visit_Assignment(self, node):
        return c_ast.Assignment(
            node.op,
            self._safe_visit(node.lvalue),
            self._safe_visit(node.rvalue),
            node.coord
        )

    def visit_BinaryOp(self, node):
        if (node.op in ["&&", "||"]) and self.rm_boolops:
            return c_ast.FuncCall(
                c_ast.ID(self._get_binop_func(node.op)),
                c_ast.ExprList([
                    self._safe_visit(node.left),
                    self._safe_visit(node.right)
                ]),
                node.coord
            )
        return c_ast.BinaryOp(
            node.op,
            self._safe_visit(node.left),
            self._safe_visit(node.right),
            node.coord
        )

    # def visit_Break(self, node):
    #     return node

    def visit_Case(self, node):
        return c_ast.Case(
            self._safe_visit(node.expr),
            map(self._safe_visit, node.stmts),
            node.coord
        )

    def visit_Cast(self, node):
        return c_ast.Cast(
            node.to_type,
            self._safe_visit(node.expr),
            node.coord
        )

    def visit_Compound(self, node):
        return c_ast.Compound(
            self._safe_map(self._safe_visit, node.block_items),
            node.coord
        )

    def visit_CompoundLiteral(self, node):
        return c_ast.CompoundLiteral(
            node.type,
            self._safe_visit(node.init),
        )

    # def visit_Constant(self, node):
    #     return node

    # def visit_Continue(self, node):
    #     return node

    def visit_Decl(self, node):
        return c_ast.Decl(
            node.name,
            node.quals,
            node.align,
            node.storage,
            node.funcspec,
            self._safe_visit(node.type),
            node.init,
            node.bitsize,
            node.coord
        )

    def visit_DeclList(self, node):
        return c_ast.DeclList(
            map(self._safe_visit, node.decls),
            node.coord
        )

    def visit_Default(self, node):
        return c_ast.Default(
            map(self._safe_visit, node.stmts),
            node.coord
        )

    def visit_DoWhile(self, node):
        # _ = c_ast.FuncCall(
        #     c_ast.ID("IFG"),
        #     c_ast.ExprList([
        #         self._safe_visit(node.cond),
        #         c_ast.Constant("int", str(self._fresh_int()))
        #     ]),
        #     node.coord
        # )
        return c_ast.DoWhile(
            self._safe_visit(node.cond),
            self._safe_visit(node.stmt),
            node.coord
        )

    # def visit_EllipsisParam(self, node):
    #     return node

    # def visit_EmptyStatement(self, node):
    #     return node

    # def visit_Enum(self, node):
    #     return node

    # def visit_Enumerator(self, node):
    #     return node

    # def visit_EnumeratorList(self, node):
    #     return node

    def visit_ExprList(self, node):
        return c_ast.ExprList(
            map(self._safe_visit, node.exprs),
            node.coord
        )

    def visit_FileAST(self, node):
        return c_ast.FileAST(
            map(self._safe_visit, node.ext),
            node.coord
        )

    def visit_For(self, node):
        # _ = c_ast.FuncCall(
        #     c_ast.ID("IFG"),
        #     c_ast.ExprList([
        #         self._safe_visit(node.cond),
        #         c_ast.Constant("int", str(self._fresh_int()))
        #     ]),
        #     node.coord
        # )
        return c_ast.For(
            self._safe_visit(node.init),
            self._safe_visit(node.cond),
            self._safe_visit(node.next),
            self._safe_visit(node.stmt),
            node.coord
        )

    def visit_FuncCall(self, node):
        tmp = self.rm_boolops
        self.rm_boolops = True
        n_args = self._safe_visit(node.args)
        self.rm_boolops = tmp
        return c_ast.FuncCall(
            node.name,
            n_args,
            node.coord
        )

    def visit_FuncDecl(self, node):
        n_args = node.args
        if self.ctx[-1] == "main" and n_args:
            n_args = None
        return c_ast.FuncDecl(n_args, node.type, node.coord)

    def visit_FuncDef(self, node):
        self.ctx.append(node.decl.name)
        n_decl = self._safe_visit(node.decl)
        self.ctx.pop()
        return c_ast.FuncDef(
            n_decl,
            node.param_decls,
            self._safe_visit(node.body),
            node.coord
        )

    # def visit_Goto(self, node):
    #     return node

    # def visit_ID(self, node):
    #     return node

    # def visit_IdentifierType(self, node):
    #     return node

    def visit_If(self, node):
        # _ = c_ast.FuncCall(
        #     c_ast.ID("IFG"),
        #     c_ast.ExprList([
        #         self._safe_visit(node.cond),
        #         c_ast.Constant("int", str(self._fresh_int()))
        #     ]),
        #     node.coord
        # )
        return c_ast.If(
            self._safe_visit(node.cond),
            self._safe_visit(node.iftrue),
            self._safe_visit(node.iffalse),
            node.coord
        )

    def visit_InitList(self, node):
        return c_ast.InitList(
            map(self._safe_visit, node.exprs),
            node.coord
        )

    def visit_Label(self, node):
        return c_ast.Label(
            node.name,
            self.visit(node.stmt),
            node.coord,
        )

    def visit_NamedInitializer(self, node):
        return c_ast.NamedInitializer(
            map(self._safe_visit, node.name),
            self._safe_visit(node.expr),
            node.coord
        )

    def visit_ParamList(self, node):
        return c_ast.ParamList(
            map(self._safe_visit, node.params),
            node.coord
        )

    # def visit_PtrDecl(self, node):
    #     return node

    def visit_Return(self, node):
        return c_ast.Return(
            self._safe_visit(node.expr),
            node.coord
        )

    # def visit_Struct(self, node):
    #     return node

    # def visit_StructRef(self, node):
    #     return node

    def visit_Switch(self, node):
        # _ = c_ast.FuncCall(
        #     c_ast.ID("IFG"),
        #     c_ast.ExprList([
        #         self._safe_visit(node.cond),
        #         c_ast.Constant("int", str(self._fresh_int()))
        #     ]),
        #     node.coord
        # )
        return c_ast.Switch(
            self._safe_visit(node.cond),
            self._safe_visit(node.stmt),
            node.coord
        )

    def visit_TernaryOp(self, node):
#        return c_ast.FuncCall(
#            c_ast.ID("ite"),
#            c_ast.ExprList([
#                self._safe_visit(node.cond),
#                self._safe_visit(node.iftrue),
#                self._safe_visit(node.iffalse)
#            ]),
#            node.coord
#        )
        return c_ast.TernaryOp(
            self._safe_visit(node.cond),
            self._safe_visit(node.iftrue),
            self._safe_visit(node.iffalse),
            node.coord
        )

    # def visit_TypeDecl(self, node):
    #     return node

    # def visit_Typedef(self, node):
    #     return node

    # def visit_Typename(self, node):
    #     return node

    def visit_UnaryOp(self, node):
        return c_ast.UnaryOp(
            node.op,
            self._safe_visit(node.expr),
            node.coord
        )

    # def visit_Union(self, node):
    #     return node

    def visit_While(self, node):
        # _ = c_ast.FuncCall(
        #     c_ast.ID("IFG"),
        #     c_ast.ExprList([
        #         self._safe_visit(node.cond),
        #         c_ast.Constant("int", str(self._fresh_int()))
        #     ]),
        #     node.coord
        # )
        return c_ast.While(
            self._safe_visit(node.cond),
            self._safe_visit(node.stmt),
            node.coord
        )

    # def visit_Pragma(self, node):
    #     return node

    def generic_visit(self, node):
        return node
