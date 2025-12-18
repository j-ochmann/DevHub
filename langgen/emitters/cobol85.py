from emitters.base import Emitter

class Cobol85Emitter(Emitter):
    INDENT = "  "  # dvě mezery pro odsazení

    def emit_program(self, node, level):
        lines = []
        lines.append("IDENTIFICATION DIVISION.")
        lines.append("PROGRAM-ID. EXAMPLE.")
        lines.append("")
        lines.append("DATA DIVISION.")
        lines.append("WORKING-STORAGE SECTION.")
        # deklarace proměnných jednoduchá, integer
        lines.append(f"{self.i(1)}01 X        PIC 9(4) VALUE 0.")
        lines.append("")
        lines.append("PROCEDURE DIVISION.")
        
        for stmt in node["body"]:
            lines.append(self.emit(stmt, 1))
        
        lines.append(f"{self.i(1)}STOP RUN.")
        return "\n".join(lines)

    # -------- Statements --------
    def emit_var_decl(self, node, level):
        # COBOL deklaruje ve WORKING-STORAGE; ignorujeme zde
        return ""  # už jsme deklarovali X ručně

    def emit_assign(self, node, level):
        return f"{self.i(level)}MOVE {self.emit(node['value'])} TO {node['target'].upper()}."

    def emit_print(self, node, level):
        return f"{self.i(level)}DISPLAY {self.emit(node['value'])}."

    def emit_if(self, node, level):
        lines = []
        cond = self.emit(node["condition"])
        lines.append(f"{self.i(level)}IF {cond} THEN")

        for stmt in node["then"]:
            lines.append(self.emit(stmt, level + 1))

        lines.append(f"{self.i(level)}END-IF")

        return "\n".join(lines)

    # -------- Expressions --------
    def emit_literal(self, node, level=0):
        return str(node["value"])

    def emit_identifier(self, node, level=0):
        return node["name"].upper()

    def emit_binary(self, node, level=0):
        left = self.emit(node["left"])
        right = self.emit(node["right"])
        op = node["op"]
        # jednoduchý mapping pro COBOL
        if op == "==":
            op = "="
        elif op == "!=":
            op = "<>"
        return f"{left} {op} {right}"
