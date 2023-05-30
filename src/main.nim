import help, std/os
echo paramStr(1).changeFileExt("html")
writeFile paramStr(1).changeFileExt("html"), parse(lex readFile(paramStr(1))).analyze().toHTML()