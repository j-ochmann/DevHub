class MarkdownEmitter:
    def __init__(self, code_emitters):
        self.code_emitters = code_emitters

    def emit_program(self, ast):
        lines = []

        # ---- kompletní program ----
        lines.append("## Kompletní program\n")
        for lang, emitter in self.code_emitters.items():
            lines.append(f"### {lang.capitalize()}")
            lines.append("```" + lang)
            lines.append(emitter.emit(ast))
            lines.append("```")
            lines.append("")

        # ---- učebnicové sekce ----
        for node in ast["body"]:
            if node["type"] == "if":
                lines.append(self.emit_if(node))

        return "\n".join(lines)

    def emit_if(self, node):
        out = []
        out.append("## Podmínka `if`")
        out.append("")
        out.append("Podmíněné vykonání kódu při splnění podmínky.")
        out.append("")

        for name, emitter in self.code_emitters.items():
            code = emitter.emit(
                {
                    "type": "program",
                    "body": [node]
                }
            )
            out.append(f"### {name}")
            out.append("```" + name)
            out.append(code)
            out.append("```")
            out.append("")

        return "\n".join(out)

