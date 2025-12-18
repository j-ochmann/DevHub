from emitters.base import Emitter

class Fortran77Emitter(Emitter):
    INDENT = "  "  # FORTRAN má malé odsazení

    def emit_program(self, node, level):
        lines = []
        lines.append("      PROGRAM EXAMPLE")

        for stmt in node["body"]:
            lines.append(self.emit(stmt, 1))

        lines.append("      END")
        return "\n".join(lines)

    def emit_literal(self, node, level=0):
        return str(node["value"])

    def emit_identifier(self, node, level=0):
        return node["name"].upper()

    def emit_binary(self, node, level=0):
        return f"{self.emit(node['left'])} {node['op']} {self.emit(node['right'])}"

    def emit_print(self, node, level):
        return f"{self.i(level)}PRINT *, {self.emit(node['value'])}"

    def emit_if(self, node, level):
        lines = []
        cond = self.emit(node["condition"])

        lines.append(f"{self.i(level)}IF ({cond}) THEN")

        for stmt in node["then"]:
            lines.append(self.emit(stmt, level + 1))

        lines.append(f"{self.i(level)}ENDIF")

        return "\n".join(lines)
