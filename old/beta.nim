import std/[os, sequtils, strutils, tables]

echo AllChars
let file = open(paramStr(1))

type
    Tkname {.pure.} = enum
        default
        ident,
        node,
        operator,
        seperator,
        number,
        `string`,
        group,
        body,
        args
    Token = object
        name: Tkname
        value: string
    ASTNode = object
        head: Token
        body: seq[ref ASTNode]
    AST = ref ASTNode

method reset(tkn:var Token) {.base.} =
    tkn.name = Tkname.default
    tkn.value = ""

var 
    lines: seq[string] = @[]
    result: seq[tuple[indent: uint, line: string]]
    extraRecord = ""
    current_token: Token
    lexed_tokens: seq[Token]
    parsed_ast: AST = new AST
    last_asts: seq[AST] = @[parsed_ast]

proc indof(str: string): tuple[indent: uint, line: string] =
    result.indent = 0
    result.line = str
    var current: seq[char] = cast[seq[char]](str)
    for ch in str:
        if ch == ' ':
            result.indent += 1
            current.delete(0)
        else:
            result.line = cast[string](current)
            break
    return result

while(true):
    try:
        lines.add(readLine(file))
    except IOError:
        break

for v in lines:
    result.add(indof(v))

const
    Operators = ["+", "-", "*", "/", "="]
    Seperators = [':', '(', ')']

let Nodes = {
    "body": (
        args: newSeq[string](0),
        body: @["<body>", "$content", "\n</body>"]
    ),
    "header": (
        args: @["$n"],
        body:  @["\n<h", "$n", ">\n", "$content", "\n</h", "$n", ">"]
    ),
    "style": (
        args: @[],
        body: @["<style>", "$content", "\n</style>"]
    ),
    "id": (
        args: @["$id"],
        body: @["<div id=\"", "$id", "\"", ">", "$content", "\n</div>\n"],
    ),
    "title": (
        args: @["$title"],
        body: @["<title>", "$title", "\n</title>"]
    ),
    "code": (
        args: newSeq[string](0),
        body: @["<code>", "$content", "\n</code>"]
    )
}.toTable()

let StyleNodes = {
    "body": @[
        "\nbody {",
        "$content",
        "\n}"
    ],
    "header": @[
        "\nh", "$n", " {",
        "$content",
        "\n}"
    ],
    "id": @[
        "\n#", "$id", "{",
        "$content",
        "\n}"
    ]
}.toTable()

let StyleArgs = {
    "body": @[],
    "header": @[
        "$n"
    ],
    "id": @[
        "$id"
    ]
}.toTable()

let Macros = {
    "rgb": (
        args: @[
            "$red",
            "$green",
            "$blue"
        ],
        html: @[
            "rgb(",
            "$red",
            ", ",
            "$green",
            ", ",
            "$blue",
            ")"
        ]
    )
}.toTable()
proc handleInfo() =
    if extraRecord != "":
        if extraRecord in Operators:
            lexed_tokens.add(Token(
                name:  Tkname.operator,
                value: extraRecord
            ))
        else:
            var possibleOp = ""
            for x in extraRecord:
                if x in Seperators:
                    if possibleOp in Operators:
                        lexed_tokens.add(Token(
                            name:  Tkname.operator,
                            value: possibleOp
                        ))
                        possibleOp = ""
                    lexed_tokens.add(Token(
                        name:  Tkname.seperator,
                        value: $x
                    ))
                else:
                    possibleOp.add(x)
        extraRecord = ""

proc default_tk(c:char) =
    case c
    of Whitespace:
        handleInfo()
    of IdentStartChars:
        handleInfo()
        current_token.name = Tkname.ident
        current_token.value = $c
    of Digits:
        handleInfo()
        current_token.name = Tkname.number
        current_token.value = $c
    of '!', '$', '%', '&', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', '\\', ']', '^', '`', '{', '|', '}', '~':
        extraRecord.add(c)
    of '\"':
        handleInfo()
        current_token.name = Tkname.string
    else:
        discard

proc ident_tk(c:char) =
    case c
    of IdentChars:
        current_token.value.add(c)
    of Whitespace:
        current_token.name = (if Nodes.hasKey(current_token.value) or StyleNodes.hasKey(current_token.value): Tkname.node
        else: Tkname.ident)
        lexed_tokens.add(current_token)
        current_token.reset()
    of '!', '$', '%', '&', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', '\\', ']', '^', '`', '{', '|', '}', '~':
        current_token.name = (if Nodes.hasKey(current_token.value) or StyleNodes.hasKey(current_token.value): Tkname.node
        else: Tkname.ident)
        lexed_tokens.add(current_token)
        current_token.reset()
        extraRecord.add(c)
    else:
        discard

proc number_tk(c:char) =
    case c
    of Digits:
        current_token.value.add(c)
    of '!', '$', '%', '&', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', '\\', ']', '^', '`', '{', '|', '}', '~':
        lexed_tokens.add(current_token)
        current_token.reset()
        extraRecord.add(c)
    else:
        lexed_tokens.add(current_token)
        current_token.reset()

proc string_tk(c:char) =
    if c == '\"':
        if current_token.value[^1] == '\\':
            current_token.value[^1] = c
        else:
            lexed_tokens.add(current_token)
            current_token.reset()
    else:
        current_token.value.add(c)

var inds: seq[tuple[indent: uint, line: string]]

proc new_ast(x:Token): AST =
    new result
    result.head = x

proc echoAST(x:AST, step = 4, ind = 0) =
    if x.body.len() == 0:
        echo indent($x.head, ind)
    else:
        echo indent($x.head, ind) & " {"
        for ch in x.body:
            echoAST ch, step, ind + step
        echo indent("}", ind)

template insertToAST() =
    echo lexed_tokens[lastlexed..^1]
    for idx, x in lexed_tokens[lastlexed..^1]:
        echo x
        last_asts[^1].body.add(new_ast(x))
        if idx + lastlexed == lexed_tokens.high:
            if x == Token(name: Tkname.seperator, value: ":") and last_asts.len() > 1:
                last_asts[^1].body.delete(last_asts[^1].body.high)
        if x == Token(name: Tkname.seperator, value: "("):
            if lexed_tokens[lastlexed + idx - 1].name == Tkname.ident:
                last_asts[^1].body.delete(last_asts[^1].body.high)
            else:
                last_asts[^1].body[^1] = new_ast(Token(name: Tkname.group, value: ""))
            last_asts.add(last_asts[^1].body[^1])
        elif x == Token(name: Tkname.seperator, value: ")"):
            last_asts[^1].body.delete(last_asts[^1].body.high)
            last_asts.delete(last_asts.high)
            if last_asts[^1].head == Token(name: Tkname.operator, value: "="):
                last_asts.delete(last_asts.high)
        elif x == Token(name: Tkname.operator, value: "="):
            last_asts[^1].body[^1].body.add(last_asts[^1].body[^2])
            last_asts[^1].body.delete(last_asts[^1].body.high - 1)
            last_asts.add(last_asts[^1].body[^1])
        elif x.name == Tkname.node:
            last_asts.add(last_asts[^1].body[^1]) 

for x in lines:
    inds.add(indof(x))
    let lastlexed = lexed_tokens.len()
    for x in inds[^1].line & " ":
        case current_token.name
        of Tkname.default:
            default_tk(x)
        of Tkname.ident:
            ident_tk(x)
        of Tkname.number:
            number_tk(x)
        of Tkname.string:
            string_tk(x)
        else:
            discard
    if current_token.name == Tkname.string:
        current_token.value.add('\n')
    if inds.len() > 1:
        if inds[^2].indent == inds[^1].indent:
            insertToAST()
        else:
            if inds[^2].indent > inds[^1].indent:
                last_asts.delete(last_asts.high)
                if last_asts.len() > 1:
                    last_asts.delete(last_asts.high)
                insertToAST()  
            else:
                last_asts[^1].body.add(new_ast(Token(name: Tkname.body, value: "")))
                last_asts.add(last_asts[^1].body[^1])
                insertToAST()
    else:
        insertToAST()

echoAST(parsed_ast)

proc toCSS(ast:AST, ind = 0, step = 2): string

proc htmlMacro(ast:AST): string =
    if ast.head.name in [Tkname.number, Tkname.string]:
        result = ast.head.value
    else:
        for x in Macros[ast.head.value].html:
            if x[0] == '$':
                result = result & ast.body[Macros[ast.head.value].args.find(x)].htmlMacro()
            else:
                result = result & x

proc toHTML(ast:AST, ind = 0, step = 2): string =
    if ast.head.name == Tkname.number:
        result = ast.head.value
    elif ast.head.name == Tkname.string:
        result = ast.head.value.replace("<", "&lt;").replace(">", "&gt;")
    else:
        for x in ast.body:
            if x.head.name == Tkname.node:
                for y in Nodes[x.head.value].body:
                    if y == "$content":
                        if x.head.value == "style":
                            result = result & toCSS(if x.body[0].head.name == Tkname.body: x.body[0] else: x.body[1], ind+step, step)
                        else:
                            result = result & toHTML(if x.body[0].head.name == Tkname.body: x.body[0] else: x.body[1], ind+step, step)
                    elif y[0] == '$':
                        echo y
                        echo Nodes[x.head.value].args
                        result = result & toHTML(x.body[Nodes[x.head.value].args.find(y)], ind+step, step)
                    else:
                        result = result & y
            elif x.head.name == Tkname.number:
                result = result & x.head.value
            elif x.head.name == Tkname.string:
                result = result & x.head.value.replace("<", "&lt;").replace(">", "&gt;")

proc toCSS(ast:AST, ind = 0, step = 2): string =
    if ast.head.name in [Tkname.number, Tkname.string, Tkname.number]:
        result = ast.head.value
    else:
        for x in ast.body:
            if x.head.name == Tkname.node:
                for y in StyleNodes[x.head.value]:
                    if y == "$content":
                        result = result & toCSS(if x.body[0].head.name == Tkname.body: x.body[0] else: x.body[1], ind+step, step)
                    elif y[0] == '$':
                        result = result & toCSS(x.body[0].body[StyleArgs[x.head.value].find(y)], ind+step, step)
                    else:
                        result = result & y
            elif x.head == Token(name: Tkname.operator, value: "="):
                echoAST x.body[1]
                result = result & "\n" & x.body[0].head.value & ": " & (if x.body[1].head.value in Macros: x.body[1].htmlMacro() else: x.body[1].head.value)
            elif x.head.name in [Tkname.number, Tkname.string, Tkname.number]:
                result = result & x.head.value

echoAST parsed_ast
try:
    if paramStr(2) == "-o":
        writeFile paramStr(3), toHTML(parsed_ast)
    else:
        writeFile splitFile(paramStr(1)).name & ".html", toHTML(parsed_ast)
except:
    writeFile splitFile(paramStr(1)).name & ".html", toHTML(parsed_ast)