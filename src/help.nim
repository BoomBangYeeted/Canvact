import std/[sequtils, streams, strformat, strutils, tables]
import yaml

type
    TokenType {.pure.} = enum
        default,
        `char`,
        `string`,
        ident,
        keyword,
        operator,
        seperator,
        integer,
        newline
    Token = object
        tktype: TokenType
        lexeme: string
    ASTInfo {.pure.} = enum
        default
        parens
        body
        pair
        `func`
    ASTNode = object
        head*:Token
        body*:seq[ref ASTNode]
        info:   ASTInfo
    AST = ref ASTNode
    DataType {.pure.} = enum
        unknown
        `int`,
        `float`,
        double,
        `char`,
        `string`,
        `var`
    ACTInfo {.pure.} = enum
        value
        node
        pair
        `func`
    ACTNode = object
        case info:ACTInfo
        of ACTInfo.value:
            datatype:DataType
            value:   string
        of ACTInfo.node:
            name:    string
            args:    seq[ref ACTNode]
            body:    seq[ref ACTNode]
        of ACTInfo.pair:
            pair_key:     string
            pair_value:   seq[ref ACTNode]
        of ACTInfo.func:
            func_name:  string
            func_args:  seq[ref ACTNode]
    ACTTree = ref ACTNode

            

const Seperators = [
    '[',
    ']',
    '(',
    ')',
    ',',
    ':'
]

const Operators = [
    "+=", "++", "-=", "--", "=",
    "==", "!=", "<", ">", ">=", "<=",
    "+", "-", "/", "*", "%"
]

const Keywords = [
    "hook"
]

proc lex*(inp:string): seq[Token] =
    var
        current_token: Token
        lexed_tokens:  seq[Token]
        unhandled_info: string
    proc handle_info() =
        if unhandled_info != "":
            var more_unhandled_info: string
            for x in unhandled_info & " ":
                if more_unhandled_info in Operators:
                    if more_unhandled_info & x in Operators:
                        lexed_tokens.add(Token(
                            tktype: TokenType.operator,
                            lexeme: more_unhandled_info & x
                        ))
                    else:
                        lexed_tokens.add(Token(
                            tktype: TokenType.operator,
                            lexeme: more_unhandled_info
                        ))
                    more_unhandled_info = ""
                elif x in Seperators:
                    lexed_tokens.add(Token(
                        tktype: TokenType.seperator,
                        lexeme: $x
                    ))
                else:
                    more_unhandled_info.add(x)
            unhandled_info = ""
    for x in inp & " ":
        case current_token.tktype
        of  TokenType.default:
            case x
            of  'a'..'z', 'A'..'Z', '_':
                handle_info()
                current_token.tktype = TokenType.ident
                current_token.lexeme = $x
            of  '0'..'9':
                handle_info()
                current_token.tktype = TokenType.integer
                current_token.lexeme = $x
            of  '#'..'/', ':'..'@', '{', '|', '}', '['..'^', '!', '~':
                unhandled_info.add(x)
            of  '\"':
                handle_info()
                current_token.tktype = TokenType.string
            of  ' ', '\n':
                handle_info()
                if x == '\n':
                    current_token = Token(tktype:TokenType.newline, lexeme: "0")
            else:
                discard
        of  TokenType.ident:
            case x
            of  'a'..'z', 'A'..'Z', '0'..'9', '_':
                current_token.lexeme.add(x)
            else:
                if current_token.lexeme in Keywords:
                    current_token.tktype = TokenType.keyword
                lexed_tokens.add(current_token)
                current_token.reset()
                case x
                of '\"':
                    current_token.tktype = TokenType.string
                of '\'':
                    current_token.tktype = TokenType.char
                of ' ', '\n':
                    discard
                else:
                    unhandled_info.add(x)
        of  TokenType.string:
            if x == '\"':
                lexed_tokens.add(current_token)
                current_token.reset()
            else:
                current_token.lexeme.add(x)
        of  TokenType.integer:
            case x
            of '0'..'9':
                current_token.lexeme.add(x)
            else:
                lexed_tokens.add(current_token)
                current_token.reset()
                case x
                of  '#'..'/', ':'..'@', '{', '|', '}', '['..'^', '!', '~':
                    unhandled_info.add(x)
                of  '\"':
                    current_token.tktype = TokenType.string
                of  '\n':
                    lexed_tokens.delete(lexed_tokens.high)
                    current_token = Token(tktype:TokenType.newline, lexeme: "0")
                else:
                    discard
        of  TokenType.newline:
            if x == ' ':
                current_token.lexeme = $(current_token.lexeme.parseInt() + 1)
            else:
                case x
                of  'a'..'z', 'A'..'Z', '_':
                    lexed_tokens.add(current_token)
                    current_token.reset()
                    current_token.tktype = TokenType.ident
                    current_token.lexeme = $x
                of  '0'..'9':
                    lexed_tokens.add(current_token)
                    current_token.reset()
                    current_token.tktype = TokenType.integer
                    current_token.lexeme = $x
                of  '#'..'/', ':'..'@', '{', '|', '}', '['..'^', '!', '~':
                    lexed_tokens.add(current_token)
                    current_token.reset()
                    unhandled_info.add(x)
                of  '\"':
                    lexed_tokens.add(current_token)
                    current_token.reset()
                    current_token.tktype = TokenType.string
                of  '\n':
                    current_token = Token(tktype:TokenType.newline, lexeme: "0")
                else:
                    discard
        else:
            discard
    return lexed_tokens

proc new_ast(x:Token): AST =
    result = new AST
    result.head = x

proc toStr(ast:AST, ind: Natural = 0, step = 4): string =
    try:
        if ast.body.len() == 0:
            return ($(ast.head)).indent(ind)
        else:
            return ($ast.head).indent(ind) & " => " & $ast.info & " {\n" & ast.body.map(proc(x:AST): string = toStr(x, ind+step, step)).foldl(a & "\n" & b) & "\n}".indent(ind)
    except:
        return ""

proc `$`*(ast:AST): string = toStr(ast)

proc toStr(ast:ACTtree, ind: Natural = 0, step = 4): string =
    case ast.info
    of   ACTInfo.value:
        return "(value: \"".indent(max(ind - step, 0)) & ast.value & "\", datatype: " & $ast.datatype & ")"
    of   ACTInfo.node:
        return "(name: \"".indent(ind) & ast.name & "\", args: " & $(if ast.args.len() == 0: "@[]"
                   else: "@[" & ast.args.mapIt(toStr(it, step)).foldl(a & ", " & b)) & "]" & ")" & $(if ast.body.len() == 0: ""
                   else: ", body: {" & "\n".indent(ind + step) & ast.body.mapIt(toStr(it, ind+step, step)).foldl(a & "\n".indent(ind) & b) & "\n}".indent(ind)) 
    of    ACTInfo.pair:
        return "(key: \"".indent(ind) & ast.pair_key & "\", value: " & $(if ast.pair_value.len() == 0: ""
                   else: "\n".indent(ind) & ast.pair_value.mapIt(toStr(it, ind+step, step)).foldl(a & "\n".indent(ind) & b) & "\n}".indent(ind)) 
    of    ACTInfo.func:
        return "(name: ".indent(ind) & ast.func_name & ", args:" & " \n".indent(ind) & ast.func_args.mapIt(toStr(it, ind+step, step)).foldl(a & "\n".indent(ind) & b) & "\n}".indent(ind)
proc `$`*(ast:ACTtree): string = toStr(ast)


template until(pred:untyped, body:untyped) =
    while(not pred):
        body

proc id[T](v:T): T = v

proc parse*(lexed:seq[Token]): AST =
    var
        parsed_ast: AST     = new AST
        last_asts: seq[AST] = @[parsed_ast]
        blocks: seq[tuple[indent:int, index:int]] = @[(0,0)]
        groups:    seq[int] = @[0]
        skip   :   bool = false
    template possible_args(): AST =
        (if last_asts[^1].head == Token(tktype:TokenType.default, lexeme: ""):
            last_asts[^1].body[^1] 
        else: last_asts[^1])
    for idx, x in lexed:
        if skip:
            skip = false
            continue
        case x.tktype
        of  TokenType.keyword:
            last_asts[^1].body.add(new_ast x)
            last_asts.add(last_asts[^1].body[^1])
        of  TokenType.ident, TokenType.string, TokenType.integer, TokenType.char:
            last_asts[^1].body.add(new_ast x)
        of  TokenType.seperator:
            case x.lexeme
            of "(", "[":
                if last_asts[^1].body[^1].head.tktype == TokenType.ident:
                    last_asts[^1].body[^1].info = ASTInfo.func
                else:
                    last_asts[^1].body.add(new AST)
                    last_asts[^1].body[^1].info = ASTInfo.parens
                last_asts.add(last_asts[^1].body[^1])
                groups.add(last_asts.high)   
            of ")", "]":
                last_asts = last_asts[0..groups[^1]]
                if lexed[idx + 1] != Token(tktype: TokenType.seperator, lexeme: ":"):
                    groups.delete(groups.high)

            of ":":
                if lexed[idx + 1].tktype == TokenType.newline:
                    if possible_args().body.len() > 0:
                        possible_args().body.add(new AST)
                        possible_args().body[^1].info = ASTInfo.parens
                        possible_args().body[^1].body = possible_args().body[0..^2]
                        possible_args().body = possible_args().body[^1..^1]
                        last_asts = last_asts[0..blocks[^1].index]
                        last_asts.add(last_asts[^1].body[^1])
                        last_asts[^1].body.add(new AST)
                        last_asts.add(last_asts[^1].body[^1])
                        last_asts[^1].info = ASTInfo.body
                        groups.delete(groups.high)
                        blocks.add((indent: lexed[idx + 1].lexeme.parseInt(), index: last_asts.high))
                    else:
                        last_asts.add(last_asts[^1].body[^1])
                        last_asts[^1].body.add(new AST)
                        last_asts[^1].body[^1].info = ASTInfo.pair
                        last_asts.add(last_asts[^1].body[^1])
                        last_asts[^1].info = ASTInfo.body
                        blocks.add((indent: lexed[idx + 1].lexeme.parseInt(), index: last_asts.high))
                else:
                    last_asts[^1].body.add(new AST)
                    last_asts[^1].body[^1].info = ASTInfo.pair
                    last_asts[^1].body[^1].body.add(last_asts[^1].body[^2])
                    last_asts[^1].body.delete(last_asts[^1].body.high - 1)
                    last_asts.add(last_asts[^1].body[^1])
            of ",":
                last_asts = last_asts[0..groups[^1]]
        of  TokenType.newline:
            if x.lexeme.parseInt() < blocks[^1].indent:
                until(x.lexeme.parseInt() > blocks[^1].index or blocks.len() == 1):
                    blocks.delete(blocks.high)
                last_asts = last_asts[0..blocks[^1].index]
        else:
            discard

    return parsed_ast

proc analyze*(parsed:AST): ACTTree =
    var last_asts: seq[AST] = @[parsed]
    result = ACTTree(info: ACTInfo.node)
    var last_acts: seq[ACTTree] = @[result]
    var stop = false
    echo parsed
    if parsed.body.len() == 0:
        case parsed.info
        of ASTInfo.default:
            return ACTTree(info: ACTInfo.value, value: parsed.head.lexeme, datatype: (case parsed.head.tktype
                of TokenType.char:    DataType.char
                of TokenType.string:  DataType.string
                of TokenType.integer: DataType.int
                of TokenType.ident:   DataType.var
                else:                 Datatype.unknown))
        of ASTInfo.pair:
            return ACTTree(info: ACTInfo.pair, pair_key: parsed.body[0].head.lexeme, pair_value: parsed.body[1].body.map(proc(x:AST): ACTTree = x.analyze()))
        of ASTInfo.func:
            echo parsed.body
            return ACTTree(info: ACTInfo.func, func_name: parsed.head.lexeme, func_args: @[])
        else:
            return new ACTTree
    else:
        if parsed.info == ASTInfo.func:
            result = ACTTree(info: ACTInfo.func, func_name: parsed.head.lexeme, func_args: parsed.body.mapIt(it.analyze()))
        else:
            until(last_asts.len() == 0):
                for x in last_asts[^1].body:
                    if x.body.len() == 0:
                        case x.head.tktype
                        of   TokenType.ident:
                            result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.var, value: x.head.lexeme))
                        of   TokenType.char:
                            result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.char, value: x.head.lexeme))
                        of   TokenType.integer:
                            result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.int, value: x.head.lexeme))
                        of   TokenType.string:
                            result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.string, value: x.head.lexeme))
                        else:
                            discard
                    else:
                        if x.body.len() == 1:
                            if x.head.tktype == TokenType.ident and x.body.mapIt(it.info) == @[ASTInfo.body]:
                                last_acts[^1].body.add(ACTTree(info: ACTInfo.node, name: x.head.lexeme, args: @[], body: @[]))
                                for y in x.body[0].body:
                                    if y.body.len() == 0:
                                        last_acts[^1].body[^1].body.add(analyze y)
                                    else:
                                        last_acts[^1].body[^1].body.add(AST(body: @[y]).analyze())
                                if last_asts.len() == 0:
                                    stop = true
                                    break
                                last_asts.delete(last_asts.high)
                            elif x.body.mapIt(it.info) == [ASTInfo.pair]:
                                echo x
                                last_acts[^1].body.add(ACTTree(info: ACTInfo.pair, pair_key: x.body[0].body[0].head.lexeme, pair_value: x.body[0].body[1].analyze().body))
                            else:
                                case x.head.tktype
                                of   TokenType.ident:
                                    result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.var, value: x.head.lexeme))
                                of   TokenType.char:
                                    result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.char, value: x.head.lexeme))
                                of   TokenType.integer:
                                    result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.int, value: x.head.lexeme))
                                of   TokenType.string:
                                    result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.int, value: x.head.lexeme))
                                else:
                                    discard
                        elif x.body.len() == 2:
                            if x.body.mapIt(it.info) == @[ASTInfo.parens, ASTInfo.body]:
                                last_acts[^1].body.add(ACTTree(info: ACTInfo.node, name: x.head.lexeme, args: x.body[0].body.mapIt(it.analyze()), body: x.body[1].analyze().body))
                                if last_asts.len() == 0:
                                    stop = true
                                    break
                                last_asts.delete(last_asts.high)
                            elif x.info == ASTInfo.pair:
                                echo x.body[1]
                                result.body.add(ACTTree(info: ACTInfo.pair, pair_key: x.body[0].head.lexeme, pair_value: @[x.body[1].analyze()]))
                            else:
                                for y in x.body:
                                    case y.head.tktype
                                    of   TokenType.ident:
                                        result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.var, value: y.head.lexeme))
                                    of   TokenType.char:
                                        result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.char, value: y.head.lexeme))
                                    of   TokenType.integer:
                                        result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.int, value: y.head.lexeme))
                                    of   TokenType.string:
                                        result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.string, value: y.head.lexeme))
                                    else:
                                        discard
                        else:
                            for y in x.body:
                                case y.info
                                of   ASTInfo.default:
                                    case y.head.tktype
                                    of   TokenType.ident:
                                        result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.var, value: y.head.lexeme))
                                    of   TokenType.char:
                                        result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.char, value: y.head.lexeme))
                                    of   TokenType.integer:
                                        result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.int, value: y.head.lexeme))
                                    of   TokenType.string:
                                        result.body.add(ACTTree(info: ACTInfo.value, datatype: DataType.string, value: y.head.lexeme))
                                    else:
                                        discard
                                of    ASTInfo.func:
                                    result.body.add(ACTTree(info: ACTInfo.func, func_name: y.head.lexeme, func_args: x.body[0].body.mapIt(
                                        case it.head.tktype
                                        of   TokenType.ident:
                                            ACTTree(info: ACTInfo.value, datatype: DataType.var, value: it.head.lexeme)
                                        of   TokenType.char:
                                            ACTTree(info: ACTInfo.value, datatype: DataType.char, value: it.head.lexeme)
                                        of   TokenType.integer:
                                            ACTTree(info: ACTInfo.value, datatype: DataType.int, value: it.head.lexeme)
                                        of   TokenType.string:
                                            ACTTree(info: ACTInfo.value, datatype: DataType.string, value: it.head.lexeme)
                                        else:
                                            new ACTTree
                                    )))
                                of     ASTInfo.pair:
                                    echo y.body[1]
                                    result.body.add(ACTTree(info: ACTInfo.pair, pair_key: y.body[0].head.lexeme, pair_value: y.body[1].analyze().body))
                                else:
                                    discard
                if stop:
                    break
                if last_asts.len() == 0:
                    break
                if last_asts[^1].body.len() == 0:
                    last_asts.delete(last_asts.high)
                else:
                    last_asts[^1].body.delete(0)

type
    NodeSectionType {.pure.} = enum
        node,
        param,
        body
    Param   = object
        sect:  NodeSectionType
        value: string
    NodeStructure = object
        args: seq[tuple[name: string, datatype: DataType, flexible: bool]]
        html: seq[Param]
        css:  seq[Param]
    FuncStructure = object
        args: seq[tuple[name: string, datatype: DataType, flexible: bool]]
        body: seq[Param]

proc node(s:string): seq[Param] =
    result = @[Param(sect: NodeSectionType.node, value: "")]
    var skip = false
    for idx, x in s:
        if skip:
            skip = false
            continue
        if x == '{':
            if s[idx + 1] == '{':
                result[^1].value.add(x)
                skip = true
            else:
                result.add(Param(sect: NodeSectionType.param, value: ""))
        elif x == '}':
            if s[idx + 1] == '}':
                result[^1].value.add(x)
                skip = true
            else:
                if result[^1].value.len() == 0:
                    result[^1].sect = NodeSectionType.body
                result.add(Param(sect: NodeSectionType.node, value: ""))
        else:
            result[^1].value.add(x)

var nodes: Table[string, NodeStructure] = {
    "body": NodeStructure(
        args: @[],
        html: node"<body>{}</body>",
        css:  node"body{{ {} }}"
    ),
    "header": NodeStructure(
        args: @[(name: "n", datatype: DataType.int, flexible: false)],
        html: node"<h{n}>{}</h{n}>",
        css:  node"h{n} {{ {} }}"
    ),
    "para": NodeStructure(
        args: @[],
        html: node"<p>{}</p>",
        css:  node"p{{ {} }}"
    )
}.toTable()
var functions: Table[string, FuncStructure] = {
    "rgb": FuncStructure(
        args: @[(name: "r", datatype: DataType.int, flexible: true), (name: "g", datatype: DataType.int, flexible: true), (name: "b", datatype: DataType.int, flexible: true)],
        body: node"rgb({r}, {g}, {b})"
    )
}.toTable()

proc parseHtmlString(s:string): string =
    var italic = false
    for x in s:
        if x == '*':
            if italic:
                italic = false
                result = result & "</i>"
            else:
                italic  = true
                result = result & "<i>"
        else:
            result.add x

proc toCSS*(tree:ACTTree): string =
    case tree.info
    of   ACTInfo.value:
        return tree.value
    of   ACTInfo.node:
        if tree.name == "":
            return tree.body.mapIt(it.toCSS()).foldl(a & b)
        else:
            for x in nodes[tree.name].css:
                case x.sect
                of   NodeSectionType.node:
                    result = result & x.value
                of   NodeSectionType.param:
                    result = result & tree.args[nodes[tree.name].args.mapIt(it.name).find(x.value)].value
                of   NodeSectionType.body:
                    result = result & tree.body.mapIt(it.toCSS()).foldl(a & "\n" & b)
    of  ACTInfo.pair:
        result = tree.pair_key & ": " & tree.pair_value.mapIt(it.toCSS()).foldl(a & b) & ";"
    of  ACTInfo.func:
        for x in functions[tree.func_name].body:
            case x.sect
            of   NodeSectionType.node:
                result = result & x.value
            of   NodeSectionType.param:
                result = result & tree.func_args[functions[tree.func_name].args.mapIt(it.name).find(x.value)].value
            of   NodeSectionType.body:
                result = result & tree.body.mapIt(it.toCSS()).foldl(a & "\n" & b)

proc toHTML*(tree:ACTTree): string =
    echo tree
    case tree.info
    of   ACTInfo.value:
        return parseHtmlString tree.value
    of   ACTInfo.node:
        if tree.name == "":
            return tree.body.mapIt(it.toHTML()).foldl(a & "\n" & b)
        else:
            if tree.name == "style":
                result =  "<style>\n" & tree.body.mapIt(it.toCSS()).foldl(a & b) & "\n</style>"
            else:
                for x in nodes[tree.name].html:
                    case x.sect
                    of   NodeSectionType.node:
                        result = result & x.value 
                    of   NodeSectionType.param:
                        result = result & tree.args[nodes[tree.name].args.mapIt(it.name).find(x.value)].value
                    of   NodeSectionType.body:
                        result = result & tree.body.mapIt(it.toHTML()).foldl(a & "\n" & b)
    of  ACTInfo.pair:
        result = tree.pair_key & ": " & tree.pair_value.mapIt(it.toHTML()).foldl(a & b) & ";"
    of  ACTInfo.func:
        for x in functions[tree.func_name].body:
            case x.sect
            of   NodeSectionType.node:
                result = result & x.value
            of   NodeSectionType.param:
                result = result & tree.func_args[functions[tree.func_name].args.mapIt(it.name).find(x.value)].value
            of   NodeSectionType.body:
                result = result & tree.body.mapIt(it.toHTML()).foldl(a & b)