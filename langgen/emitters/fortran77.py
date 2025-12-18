from emitters.base import Emitter

class Fortran77Emitter(Emitter):
    INDENT = "  "

    def emit_program(self, node, level):
        lines = []
        lines.append("      PROGRAM EXAMPLE")

        # deklarace
        for var in sorted(self.symbols):
            lines.append(f"      INTEGER {var.upper()}")

        for stmt in node["body"]:
            lines.append(self.emit(stmt, 1))

        lines.append("      END")
        return "\n".join(lines)

    # -------- Statements --------

    def emit_var_decl(self, node, level):
        # Fortran 77: implicitní INTEGER
        name = node["name"].upper()
        value = self.emit(node["value"])
        return f"{self.indent(level)}{name} = {value}"

    def emit_assign(self, node, level):
        return f"{self.indent(level)}{node['target'].upper()} = {self.emit(node['value'])}"

    def emit_print(self, node, level):
        return f"{self.indent(level)}PRINT *, {self.emit(node['value'])}"

    def emit_if(self, node, level):
        lines = []
        cond = self.emit(node["condition"])
        lines.append(f"{self.indent(level)}IF ({cond}) THEN")

        for stmt in node["then"]:
            lines.append(self.emit(stmt, level + 1))

        lines.append(f"{self.indent(level)}ENDIF")
        return "\n".join(lines)

    # -------- Expressions --------

    def emit_literal(self, node, level=0):
        return str(node["value"])

    def emit_identifier(self, node, level=0):
        return node["name"].upper()

    def emit_binary(self, node, level=0):
        left = self.emit(node["left"])
        right = self.emit(node["right"])
        return f"{left} {node['op']} {right}"
