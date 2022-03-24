# ParserInRaket
A simple math parser using Raket, for CS-441 Languages

My parser comes in 2 files ParserMK2.rkt and Scanner.rkt 
I decided to parse the calculator language from the text.
The files are dependent and need to be in the same folder to function together. 
To use the parser, open ParserMK2.rkt in drracket, click run. In the command line window call the function:     (parse “filename.txt”), the file to be parsed must be a .txt file, include the .txt, and must be in parentheses. 
Another note, the .txt file to be parsed must be saved in the same folder as that ParserMK2 and Scanner, else the path won’t be able to be resolved and the parser will throw an error message.
The only significant resources came from the book “Programming Language Pragmatics” pages p57 for the scanner and p86 for the scanner predict set. 
Note I uploaded the files multiple times so they may be labeled ParserMK2-5 or something like that. 
