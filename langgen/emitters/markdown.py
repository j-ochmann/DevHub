class MarkdownEmitter:
    def __init__(self, code_emitters):
        self.code_emitters = code_emitters

    def emit_program(self, ast):
        sections = []

        for node in ast["body"]:
            if node["type"] == "if":
                sections.append(self.emit_if(node))

        return "\n\n".join(sections)

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

